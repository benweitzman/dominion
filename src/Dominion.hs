module Dominion (
                -- | How to use: https:\/\/github.com\/egonschiele\/dominion
                module Dominion,
                Option(..),
                has, handValue, pileEmpty, getPlayer, cardsOf, validateBuy, validatePlay, getRound, countNum) where

import           Control.Applicative
import           Control.Monad          hiding (join)
import           Control.Monad.IO.Class
import           Control.Monad.State    hiding (join, state)
import           Data.Either
import           Data.List
import qualified Data.Map.Lazy          as M
import           Data.Maybe
import           Data.Ord
import qualified Dominion.Cards         as CA
import           Dominion.Internal
import           Dominion.Types         (Option (..))
import qualified Dominion.Types         as T
import           Dominion.Utils
import           Prelude                hiding (log)
import           Text.Printf

-- | Convenience function. @ name \`uses\` strategy @ is the same as writing
-- @ (name, strategy) @
uses :: String -> T.Strategy -> (T.Player, T.Strategy)
name `uses` strategy = (makePlayer name, strategy)

-- | The main method to simulate a dominion game. Example:
--
-- > import Dominion
-- > import Dominion.Strategies
--
-- > main = dominion ["adit" `uses` bigMoney, "maggie" `uses` bigMoqney]
dominion :: [(T.Player, T.Strategy)] -> IO [T.Result]
dominion = dominionWithOpts []

-- | Same as `dominion`, but allows you to pass in some options. Example:
--
-- > dominionWithOpts [Iterations 5, Log True] ["adit" `uses` bigMoney, "maggie" `uses` bigMoney]
dominionWithOpts :: [T.Option] -> [(T.Player, T.Strategy)] -> IO [T.Result]
dominionWithOpts options list = do
    results <- forM [1..iterations] $ \i -> do
      gameState <- makeGameState options (rotate i players)
      evalStateT (run $ rotate i strategies) gameState
    let winnerNames = map T.winner results
    forM_ players $ \player -> do
      let name = T.playerName player
      putStrLn $ printf "player %s won %d times" name (count name winnerNames)
    return results
  where (players, strategies) = unzip list
        iterations    = fromMaybe 1000 (findIteration options)

-- | Given a name, creates a player with that name.
makePlayer :: String -> T.Player
makePlayer name = T.Player name [] (7 `cardsOf` CA.copper ++ 3 `cardsOf` CA.estate) [] 1 1 0

makeGameState :: [T.Option] -> [T.Player] -> IO T.GameState
makeGameState options players = do
    actionCards_ <- deckShuffle CA.allActionCards
    let requiredCards = take 10 $ fromMaybe [] (findCards options)
        verbose       = fromMaybe False (findLog options)
        actionCards   = take (10 - length requiredCards) actionCards_ ++ requiredCards
        cards         = M.fromList ([(CA.copper, 60), (CA.silver, 40), (CA.gold, 30),
                                    (CA.estate, 12), (CA.duchy, 12), (CA.province, 12)]
                                    ++ [(c, 10) | c <- actionCards])
    return $ T.GameState players cards 1 verbose

gameOver :: M.Map T.Card Int -> Bool
gameOver cards
    | cards M.! CA.province == 0 = True
    | M.size (M.filter (== 0) cards) >= 3 = True
    | otherwise = False

game :: [T.Strategy] -> T.Dominion ()
game strategies = do
   state <- get
   let ids = indices $ T.players state
   forM_ (zip ids strategies) (uncurry playTurn)

run :: [T.Strategy] -> T.Dominion T.Result
run strategies = do
  game strategies
  cards <- T.cards <$> get
  if gameOver cards
    then returnResults
    else do modify $ \s -> s{T.roundNum = T.roundNum s + 1}
            run strategies

returnResults :: T.Dominion T.Result
returnResults = do
    state <- get
    let players = T.players state
    results <- zip players <$> forM (indices players) countPoints
    let winner  = T.playerName . fst $ maximumBy (comparing snd) results
    when (T.verbose state) $ do
      liftIO $ putStrLn "Game Over!"
      forM_ results $ \(player, points) -> liftIO $ putStrLn $
        printf "player %s got %d points" (T.playerName player) points
    return $ T.Result results winner

-- | Player buys a card. Example:
--
-- > playerId `buys` smithy
buys :: T.PlayerId -> T.Card -> T.Dominion T.PlayResult
buys playerId card = do
    validation <- validateBuy playerId card
    case validation of
      Just x -> return $ Just x
      Nothing -> do
                 money <- handValue playerId
                 modifyPlayer playerId $ \p -> p{T.discard=card:T.discard p
                                                 ,T.buys=T.buys p - 1
                                                 ,T.extraMoney=T.extraMoney p - T.cost card
                                                 }
                 modify $ \s -> s{T.cards=decrement card (T.cards s)}
                 log playerId $ printf "bought a %s" (T.name card)
                 return Nothing

-- | Give an array of cards, in order of preference.
-- This function will buy as many cards as possible, in order of
-- preference. For example, suppose you use:
--
-- > playerId `buysByPreference` [province, duchy]
--
-- And you have 16 money and two buys. You will buy two provinces.
-- This runs all the same validations as `buys`.
buysByPreference :: T.PlayerId -> [T.Card] -> T.Dominion ()
buysByPreference playerId cards = do
    purchasableCards <- filterM (\card -> maybeToBool <$> validateBuy playerId card) cards
    unless (null purchasableCards) $ do
      playerId `buys` head purchasableCards
      playerId `buysByPreference` cards

-- | Give an array of cards, in order of preference.
-- This function will try to play as many cards as possible, in order of preference.
-- Note: if any card requires a `Followup` (like `cellar` or
-- `chapel`), you need to use `plays` instead. This runs all the same
-- validations as `plays`.
playsByPreference :: T.PlayerId -> [T.Card] -> T.Dominion ()
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
plays :: T.Playable a => T.PlayerId -> a -> T.Dominion T.PlayResult
playerId `plays` card = do
    result <- T.play playerId card
    case result of
        Just x -> return (Just x)
        Nothing -> do modifyPlayer playerId (\p -> p{T.actions = T.actions p - 1})
                      return $ Nothing

