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
    plays,
    playsByPreference,
    uses,
    validateBuy,
    validatePlay,
    with
    )

where

import           Control.Applicative
import           Control.Monad          hiding (join)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State    hiding (join, state)
import           Data.Either
import           Data.List
import qualified Data.Map.Lazy          as M
import           Data.Maybe
import           Data.Ord
import qualified Dominion.Cards         as CA
import           Dominion.Internal
import           Dominion.Utils
import           Prelude                hiding (log)
import           System.IO
import           Text.Printf

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
dominion :: [(Player, Strategy)] -> IO [Result]
dominion = dominionWithOpts defaultOptions

-- | Same as `dominion`, but allows you to pass in some options. Example:
--
-- > dominionWithOpts [Iterations 5, Log True] ["adit" `uses` bigMoney, "maggie" `uses` bigMoney]
dominionWithOpts :: Options -> [(Player, Strategy)] -> IO [Result]
dominionWithOpts options list = do
    results <- forM [1..iterations] $ \i -> do
      gameState <- makeGameState options (rotate i players)
      let runLogger = case logLevel options of
                        Nothing -> (`runLoggingT` gatedOutput stdout LevelError)
                        Just level -> (`runLoggingT` gatedOutput stdout level)
      runLogger $ evalStateT (run $ rotate i strategies) gameState
    let winnerNames = map winner results
    forM_ players $ \player -> do
      let name = playerName player
      putStrLn $ printf "player %s won %d times" name (count name winnerNames)
    return results
  where (players, strategies) = unzip list
        iterations = numIterations options

-- | Given a name, creates a player with that name.
makePlayer :: String -> Player
makePlayer name = Player name [] (7 `cardsOf` CA.copper ++ 3 `cardsOf` CA.estate) [] 1 1 0

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
    return $ GameState players cards 1 verbose []

gameOver :: M.Map CardWrap Int -> Bool
gameOver cards
    | cards M.! CA.province == 0 = True
    | M.size (M.filter (== 0) cards) >= 3 = True
    | otherwise = False

game :: [Strategy] -> Dominion ()
game strategies = do
   state <- get
   let ids = indices $ players state
   forM_ (zip ids strategies) (uncurry playTurn)

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
      lift $ printMultiGraph (80, 20) $ map (\(pid, (player, points)) -> (vps M.! pid, head $ playerName player)) pidToResult
    return $ Result results winner

-- | Player buys a card. Example:
--
-- > playerId `buys` smithy
buysCard :: PlayerId -> CardWrap -> Dominion PlayResult
buysCard playerId card = do
    validation <- validateBuy playerId card
    case validation of
      Just x -> return $ Just x
      Nothing -> do
                 money <- handValue playerId
                 modifyPlayer playerId $ \p -> p{discard=card:discard p
                                                ,buys=buys p - 1
                                                ,extraMoney=extraMoney p - cost card
                                                }
                 modify $ \s -> s{cards=decrement card (cards s)}
                 log playerId $ printf "bought a %s" (name card)
                 return Nothing

-- | Give an array of cards, in order of preference.
-- This function will buy as many cards as possible, in order of
-- preference. For example, suppose you use:
--
-- > playerId `buysByPreference` [province, duchy]
--
-- And you have 16 money and two buys. You will buy two provinces.
-- This runs all the same validations as `buys`.
buysByPreference :: PlayerId -> [CardWrap] -> Dominion ()
buysByPreference playerId cards = do
    purchasableCards <- filterM (\card -> maybeToBool <$> validateBuy playerId card) cards
    unless (null purchasableCards) $ do
      playerId `buysCard` head purchasableCards
      playerId `buysByPreference` cards

-- | Give an array of cards, in order of preference.
-- This function will try to play as many cards as possible, in order of preference.
-- Note: if any card requires a `Followup` (like `cellar` or
-- `chapel`), you need to use `plays` instead. This runs all the same
-- validations as `plays`.
playsByPreference :: PlayerId -> [CardWrap] -> Dominion ()
playsByPreference playerId cards = do
    playableCards <- filterM (\card -> maybeToBool <$> validatePlay playerId card) cards
    unless (null playableCards) $ do
      playerId `plays` head playableCards
      playerId `playsByPreference` cards

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
plays :: Playable a => PlayerId -> a -> Dominion PlayResult
playerId `plays` card = do
    result <- play playerId card
    case result of
        Just x -> return (Just x)
        Nothing -> do modifyPlayer playerId (\p -> p{actions = actions p - 1})
                      return Nothing

