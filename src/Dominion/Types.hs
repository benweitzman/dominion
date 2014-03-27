{-# LANGUAGE ExistentialQuantification, FlexibleInstances, OverlappingInstances, UndecidableInstances, MultiParamTypeClasses #-}

module Dominion.Types (
  module Dominion.Types
) where
import           Control.Monad.State
import qualified Data.Map.Lazy as M
import           Data.Monoid
import           Control.Monad.Trans.Maybe
import           Prelude             hiding (log)
import           Text.Printf

---------------------------
-- PLAYER
---------------------------

data Player = Player {
                playerName :: String,
                deck       :: [CardWrap],
                discard    :: [CardWrap],
                hand       :: [CardWrap],
                actions    :: Int,
                buys       :: Int,
                -- | Extra money gained from an action card (like +1 money
                -- from market).
                extraMoney :: Int
} deriving Show

type PlayerId = Int

---------------------------
-- GAME STATE
---------------------------

data GameStats = GameStats {
  -- | playerId -> victory points
  victoryPoints :: M.Map PlayerId Int
}

-- | This is what keeps track of all the state in the whole game.
-- Get the round number like this:
--
-- > state <- get
-- > let roundNum = round state
data GameState = GameState {
                    players :: [Player],
                    -- | all the cards still in play.
                    cards   :: M.Map CardWrap Int,
                    -- | round number
                    roundNum   :: Int,
                    verbose :: Bool,
                    -- | stats to track over the course of the game
                    stats :: [GameStats]
} 

instance Show GameState where
  show gs = "GameState {players: " ++ show (players gs)
            ++ ", cards: " ++ show (M.mapKeys show (cards gs))
            ++ ", round: " ++ show (roundNum gs)
            ++ ", verbose: " ++ show (verbose gs) ++ "}"

-- The Dominion monad is just the `StateT` monad that has a `GameState`
-- plus the IO monad.
type Dominion a = StateT GameState IO a

---------------------------
-- CARD
---------------------------
data CardType = Action
              | Attack
              | Reaction
              | Treasure
              | Victory
              | Duration
              deriving (Show, Eq, Ord)

class Card a where
  name      :: a -> String
  cost      :: a -> Int
  coinValue :: a -> Int
  types     :: a -> [CardType]
  getSetup  :: a -> (PlayerId -> Dominion PlayResult)
  getSetup _ = const mempty
  getEffect    :: a -> (PlayerId -> Dominion PlayResult)
  getEffect _ = const mempty
  getTearDown :: a -> (PlayerId -> Dominion PlayResult)
  getTearDown _ = const mempty
  points :: a -> (PlayerId -> Dominion Int)
  points _ = const $ return 0

data CardWrap = forall a . Card a => CardWrap a

instance Card CardWrap where
  name (CardWrap c) = name c
  cost (CardWrap c) = cost c
  coinValue (CardWrap c) = coinValue c
  types (CardWrap c) = types c
  getSetup (CardWrap c) = getSetup c
  getEffect (CardWrap c) = getEffect c
  getTearDown (CardWrap c) = getTearDown c
  points (CardWrap c) = points c

instance Card a => Show a where
   show c = name c

instance Card a => Ord a where
  c1 `compare` c2 = (name c1) `compare` (name c2)

instance Card a => Eq a where
  c1 == c2 = (name c1) == (name c2)

class Playable a where
  setup :: PlayerId -> a -> Dominion PlayResult
  setup _ _ = return Nothing
  effect :: PlayerId -> a -> Dominion PlayResult
  effect _ _ = return Nothing
  tearDown :: PlayerId -> a -> Dominion PlayResult
  tearDown _ _ = return Nothing
  play :: PlayerId -> a -> Dominion PlayResult
  play pid playable = mconcat $ map (($ playable) . ($ pid)) [setup, effect, tearDown]

--instance Playable Card_n where
--play pid card = _|_
--player `plays` (card `with` followup)

data Virtual = Virtual {setupFunction :: PlayerId -> Dominion PlayResult
                       ,effectFunction :: PlayerId -> Dominion PlayResult
                       ,tearDownFunction :: PlayerId -> Dominion PlayResult
                       }

instance Playable Virtual where
  setup pid (Virtual s _ _) = s pid
  effect pid (Virtual _ e _) = e pid
  tearDown pid (Virtual _ _ t) = t pid

class Effectful a b where
  with :: (Effectful a b, Playable b) => a -> b -> Virtual

instance Monoid (Dominion PlayResult) where
  mempty = return Nothing
  d1 `mappend` d2 = do r <- d1
                       case r of
                         Nothing -> d2
                         x -> return x                            

instance Card a => Playable a where
  setup pid card = (getSetup card) pid
  effect pid card = (getEffect card) pid
  tearDown pid card = (getTearDown card) pid

-- | Given a playerId, run some actions for this player. Example:
--
-- > bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]
type Strategy = PlayerId -> Dominion ()

-- | When you use a card (either you play it or you buy something),
-- you get a `PlayResult`. A `PlayResult` is either a `Left` with an error message,
-- or a `Right` with a value.
type PlayResult = Maybe String

-- | You can set these options if you use `dominionWithOpts`. Example:
--
-- > main = dominionWithOpts [Iterations 1, Log True, Cards [smithy]] ...
data Option =
            -- | Number of iterations to run.
            Iterations Int
            -- | Enable logging
            | Log Bool
            -- | A list of cards that you definitely want in the game.
            -- Useful if you are testing a strategy that relies on
            -- a particular card.
            | Cards [CardWrap]
            deriving (Show)

-- | Each `PlayerResult` is a tuple of a player and their final score.
type PlayerResult = (Player, Int)

-- | Players and their scores.
data Result = Result {
            playerResults :: [PlayerResult],
            winner        :: String
            } deriving (Show)
