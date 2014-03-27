{-# LANGUAGE ExistentialQuantification, FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module Dominion.Types (
  module Dominion.Types
) where
import           Control.Monad.State
import qualified Data.Map.Lazy as M
import           Data.Monoid
import           Control.Monad.Trans.Maybe
---------------------------
-- CARD
---------------------------

{- data CardEffect = CoinValue Int
                | VPValue Int
                | PlusCard Int
                | PlusCoin Int
                | PlusBuy Int
                | PlusAction Int
                | DurationDraw Int
                | DurationAction Int
                | DurationCoin Int
                | DurationBuy Int
                | TrashCards Int
                | TrashThisCard
                | GainCardUpto Int
                | PlayActionCard Int
                | AdventurerEffect
                | BureaucratEffect
                | CellarEffect
                | ChancellorEffect
                | GardensEffect
                | LibraryEffect
                | MineEffect
                | MoneylenderEffect
                | RemodelEffect
                | SpyEffect
                | ThiefEffect
                | OthersPlusCard Int
                | OthersDiscardTo Int
                | OthersGainCurse Int
                deriving (Show, Eq, Ord)

data Card = Card {
              _name     :: String,
              _cost     :: Int,
              _cardType :: [CardType],
              _effects  :: [CardEffect]
} deriving (Show, Eq, Ord)
makeLenses ''Card 

-- | Used with the `thief` card.
data ThiefTrashAction = TrashOnly Card | GainTrashedCard Card

-- | Some cards have a followup action associated with them. For example,
-- when you play a `workshop`, you need to choose what card you're going to
-- get. To use the followup action, you need to use the relevant data
-- constructor. See the documentation for each card to find out how to use
-- each type of `FollowupAction`.
data FollowupAction = ThroneRoom Card
                    -- | Takes a list of cards to discard.
                    | Cellar [Card]
                    -- | Boolean value representing whether you want to
                    -- move your deck into the discard pile.
                    | Chancellor Bool
                    -- | Takes a list of cards to trash.
                    | Chapel [Card]
                    -- | Takes the card you want to gain.
                    | Feast Card
                    -- | Takes the card you want to trash.
                    | Mine Card
                    -- | The first card is the card you are trashing, the
                    -- second card is the card you are gaining.
                    | Remodel (Card, Card)
                    -- | The first element is the list of cards you would discard for yourself,
                    -- the second is the lsit of cards you want others to discard.
                    | Spy ([Card], [Card])
                    -- | The function gets a list of treasure cards.
                    -- had. You return either `TrashOnly` to have the player
                    -- trash a card, or `GainTrashedCard` to gain the trashed
                    -- card. This function is called for every other
                    -- player in the game.
                    | Thief ([Card] -> ThiefTrashAction)
                    -- | Takes the card you want to gain.
                    | Workshop Card -}

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
  setup     :: a -> Virtual
  setup _ = mempty
  effect    :: a -> Virtual
  effect _ = mempty
  tearDown  :: a -> Virtual
  tearDown _ = mempty

data CardWrap = forall a . Card a => CardWrap a

instance Card CardWrap where
  name (CardWrap c) = name c
  cost (CardWrap c) = cost c
  coinValue (CardWrap c) = coinValue c
  types (CardWrap c) = types c
  setup (CardWrap c) = setup c
  effect (CardWrap c) = effect c
  tearDown (CardWrap c) = tearDown c

instance Card a => Show a where
   show c = name c

instance Card a => Ord a where
  c1 `compare` c2 = (name c1) `compare` (name c2)

instance Card a => Eq a where
  c1 == c2 = (name c1) == (name c2)

class Playable a where 
  play :: PlayerId -> a -> Dominion PlayResult
  points :: PlayerId -> a -> Dominion Int
  points _ _ = return 0

--instance Playable Card_n where
--play pid card = _|_
--player `plays` (card `with` followup)

class Effectful a where
  with :: (Effectful a, Playable b) => a -> b -> Virtual

data Virtual = Virtual {playFunction :: PlayerId -> Dominion PlayResult
                       ,pointsFunction :: PlayerId -> Dominion Int
                       }

virtual :: Virtual
virtual = Virtual (const $ return Nothing) (const $ return 0)

instance Monoid Virtual where
  mempty = virtual
  v1 `mappend` v2 = Virtual {playFunction = \pid -> do r1 <- playFunction v1 pid
                                                       case r1 of
                                                          Just x -> return $ Just x
                                                          Nothing -> playFunction v2 pid
                            ,pointsFunction = \pid -> do pointsFunction v1 pid
                                                         pointsFunction v2 pid
                            }

instance Playable Virtual where
  play pid (Virtual f _) = f pid
  points pid (Virtual _ f) = f pid

instance Card a => Playable a where
  play pid card = play pid (setup card <> effect card <> tearDown card) 
  points pid card = points pid (effect card)

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
