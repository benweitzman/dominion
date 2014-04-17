{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How to use: https:\/\/github.com\/egonschiele\/dominion
module Dominion (
    buysCard,
    buysByPreference,
    cardsOf,
    countNum,
    dominion,
    dominionWithOpts,
    getPlayer,
    getRound,
    has,
    handValue,
    Options(..),
    pileEmpty,
    playsCard,
    playsCard_,
    playsByPreference,
    uses,
    validateBuy,
    validatePlay,
    Dominion (..),
    makeGameState,
    makePlayer,
    defaultOptions,
    HasCardInfo (..),
    Effectful (..)
    )

where

import           Control.Applicative
import           Control.Monad          hiding (join)
import           Control.Monad.Logger
import           Control.Monad.State    hiding (join, state)
import           Control.Monad.Error
import           Data.Either
import           Data.List
import qualified Data.Map.Lazy          as M
import           Data.Maybe
import           Data.Ord
import Dominion.Card
import qualified Dominion.Cards         as CA
import           Dominion.Internal
import Dominion.Player
import Dominion.Types
import           Dominion.Util
import           Prelude                hiding (log)
import           System.IO
import           Text.Printf
import qualified Data.Text as T

data Options = Options {
  numIterations     :: Int,
  requiredCardNames :: [String],
  verboseOutput     :: Bool,
  logLevel          :: Maybe LogLevel
}

defaultOptions :: Options
defaultOptions = Options 1000 [] False Nothing

-- | Convenience function. @ name \`uses\` strategy @ is the same as writing
-- @ (name, strategy) @
uses :: String -> Strategy -> (Player, Strategy)
name `uses` strategy = (makePlayer name, strategy)

-- | The main method to simulate a dominion game. Example:
--
-- > import Dominion
-- > import Dominion.Strategies
--
-- > main = dominion ["adit" `uses` bigMoney, "maggie" `uses` bigMoqney]
dominion :: [(Player, Strategy)] -> IO (Maybe [Result])
dominion = dominionWithOpts defaultOptions

-- | Same as `dominion`, but allows you to pass in some options. Example:
--
-- > dominionWithOpts [Iterations 5, Log True] ["adit" `uses` bigMoney, "maggie" `uses` bigMoney]
dominionWithOpts :: Options -> [(Player, Strategy)] -> IO (Maybe [Result])
dominionWithOpts options list = do
    results <- forM [1..iterations] $ \i -> do
      gameState <- makeGameState options (rotate i players)
      let runLogger = case logLevel options of
                        Nothing -> (`runLoggingT` gatedOutput stdout LevelError)
                        Just level -> (`runLoggingT` gatedOutput stdout level)
      runLogger $ evalStateT (runErrorT . runDominion $ (run $ rotate i strategies)) gameState
    case sequence results of
      Left x -> print x >> return Nothing
      Right x-> do
        let winnerNames = map winner x
        forM_ players $ \player -> do
          let name = playerName player
          putStrLn $ printf "player %s won %d times" name (count name winnerNames)
        return $ Just x
  where (players, strategies) = unzip list
        iterations = numIterations options

-- | Given a name, creates a player with that name.
makePlayer :: String -> Player
makePlayer name = Player
    { playerName = name
    , discard =  (7 `cardsOf` CA.copper ++  3 `cardsOf` CA.estate)
    , deck = []
    , hand = []
    , played = []
    , actions = 1
    , buys = 1
    , money = 0
    }

makeGameState :: Options -> [Player] -> IO GameState
makeGameState options players = do
    actionCards_ <- deckShuffle CA.allActionCards
    let requiredCards = take 10 . mapMaybe toCard $ requiredCardNames options
        verbose       = verboseOutput options
        actionCards   = take (10 - length requiredCards) actionCards_ ++ requiredCards
        cards         = M.fromList ([(CA.copper, 60), (CA.silver, 40), (CA.gold, 30),
                                    (CA.estate, 12), (CA.duchy, 12), (CA.province, 12)]
                                    ++ [(c, 10) | c <- actionCards])
        toCard s = find ((== s) . name) actionCards_
    return $ GameState 
              { cards = cards
              , players = players
              , roundNum = 1
              , verbose = verbose
              , stats = []
              }

gameOver :: M.Map (VirtualCard ()) Int -> Bool
gameOver cards
    | cards M.! CA.province == 0 = True
    | M.size (M.filter (== 0) cards) >= 3 = True
    | otherwise = False

game :: [Strategy] -> Dominion ()
game strategies = do
   state <- get
   let ids = indices $ players state
       idsAndStrategies = zip ids strategies
   forM_ idsAndStrategies (uncurry playTurn)

run :: [Strategy] -> Dominion Result
run strategies = do
  game strategies
  cards <- cards <$> get
  if gameOver cards
    then returnResults
    else do stat <- collectStats
            modify $ \s -> s{roundNum = roundNum s + 1,stats=stats s ++ [stat]}
            run strategies

returnResults :: Dominion Result
returnResults = do
    state <- get
    let ps = players state
    results <- zip ps <$> forM (indices ps) countPoints
    let winner  = playerName . fst $ maximumBy (comparing snd) results
        stat = stats state
        vps = collate (map victoryPoints stat)
        pidToResult = zip (indices ps) results
    when (verbose state) $ do
      liftIO $ putStrLn "Game Over!"
      forM_ pidToResult $ \(pid, (player, points)) -> liftIO $ putStrLn $ printf "player %s got %d points" (playerName player) points
      printMultiGraph (80, 20) $ map (\(pid, (player, points)) -> (vps M.! pid, head $ playerName player)) pidToResult
    return $ Result results winner

-- | Player buys a card. Example:
--
-- > playerId `buys` smithy
buysCard :: PlayerId -> VirtualCard a -> Dominion Bool
buysCard playerId card = (buysCard_ playerId card >> return True) `catchError` \_ -> return False

buysCard_ :: PlayerId -> VirtualCard a -> Dominion ()
buysCard_ playerId card = do
    validateBuy playerId card 
    money <- handValue playerId
    modifyPlayer playerId $ modifyDiscard (void card:) . modifyBuys pred . modifyMoney (\x -> x - cost card)
    modify $ \s -> s{cards=decrement (void card) (cards s)}
    log playerId $ printf "bought a %s" (name card)

-- | Give an array of cards, in order of preference.
-- This function will buy as many cards as possible, in order of
-- preference. For example, suppose you use:
--
-- > playerId `buysByPreference` [province, duchy]
--
-- And you have 16 money and two buys. You will buy two provinces.s
-- This runs all the same validations as `buys`.
buysByPreference :: PlayerId -> [VirtualCard ()] -> Dominion ()
buysByPreference _ [] = return ()
buysByPreference playerId cards@(c:cs) = do 
          $(logDebug) "about to buy"
          (playerId `buysCard_` c >> playerId `buysByPreference` cards) `catchError` \_ -> playerId `buysByPreference` cs

-- | Give an array of cards, in order of preference.
-- This function will try to play as many cards as possible, in order of preference.
-- Note: if any card requires a `Followup` (like `cellar` or
-- `chapel`), you need to use `plays` instead. This runs all the same
-- validations as `plays`.
playsByPreference :: PlayerId -> [Virtual ()] -> Dominion ()
playsByPreference playerId cards@(c:cs) = do
    playerId `playsCard_` c >> playerId `playsByPreference` cards `catchError` \_ -> playerId `playsByPreference` cs

-- | In the simplest case, this lets you play a card, like this:
--
-- > playerId `plays` smithy
--
-- You can just use this function blindly, without checking to see if you
-- have enough actions, or whether you have a smithy in your hand.
-- `plays` will perform those  validations for you. It returns a `PlayResult`,
-- which is an `Either` with an error message or a return value.
--
-- Some cards require an additional action. For example, if you use
-- a workshop, you need to specify what card you're going to get. In that
-- case, this function returns a `Followup`. A `Followup` just contains some information about the card you used.
-- You can use the extra action of the card like this:
--
--  > playerId `plays` workshop `with` (Workshop gardens)
--
-- `with` takes a `FollowUp` and a `FollowupAction`, and applies the
-- `FollowupAction`.
-- Here's another example:
--
-- > playerId `plays` throneRoom `with` (ThroneRoom market)
playsCard :: Virtualizable a b => PlayerId -> a -> Dominion (Maybe b)
playerId `playsCard` card = (playerId `playsCard_` card >>= return . Just) `catchError` \_ -> return Nothing

playsCard_ :: Virtualizable a b => PlayerId -> a -> Dominion b
playerId `playsCard_` card = do 
    result <- plays playerId card
    modifyPlayer playerId $ modifyActions pred
    return result

