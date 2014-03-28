{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Dominion.Internal where
import           Control.Applicative
import           Control.Arrow
import           Control.Lens         hiding (has, indices)
import           Control.Monad        (liftM)
import           Control.Monad.Logger
import           Control.Monad.State  hiding (state)
import           Data.List
import           Data.Map.Lazy        ((!))
import qualified Data.Map.Lazy        as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Dominion.Utils
import           Prelude              hiding (log)
import           System.IO.Unsafe
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
                    players  :: [Player],
                    -- | all the cards still in play.
                    cards    :: M.Map CardWrap Int,
                    -- | round number
                    roundNum :: Int,
                    verbose  :: Bool,
                    -- | stats to track over the course of the game
                    stats    :: [GameStats]
}

instance Show GameState where
  show gs = "GameState {players: " ++ show (players gs)
            ++ ", cards: " ++ show (M.mapKeys show (cards gs))
            ++ ", round: " ++ show (roundNum gs)
            ++ ", verbose: " ++ show (verbose gs) ++ "}"

-- The Dominion monad is just the `StateT` monad that has a `GameState`
-- plus the IO monad.
type Dominion a = StateT GameState (LoggingT IO) a

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
  getSetup  :: a -> PlayerId -> Dominion PlayResult
  getSetup _ = const mempty
  getEffect    :: a -> PlayerId -> Dominion PlayResult
  getEffect _ = const mempty
  getTearDown :: a -> PlayerId -> Dominion PlayResult
  getTearDown _ = const mempty
  points :: a -> PlayerId -> Dominion Int
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
   show = name

instance Card a => Ord a where
  compare = comparing name

instance Card a => Eq a where
  c1 == c2 = name c1 == name c2

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

data Virtual = Virtual {setupFunction    :: PlayerId -> Dominion PlayResult
                       ,effectFunction   :: PlayerId -> Dominion PlayResult
                       ,tearDownFunction :: PlayerId -> Dominion PlayResult
                       }

fromCard :: Card a => a -> Virtual
fromCard card = Virtual {setupFunction = getSetup card
                        ,effectFunction = getEffect card
                        ,tearDownFunction = getTearDown card
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
  setup pid card = getSetup card pid
  effect pid card = do log pid $ printf "plays a %s!" (name card)
                       getEffect card pid
  tearDown pid card = getTearDown card pid



-- | Given a playerId, run some actions for this player. Example:
--
-- > bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]
type Strategy = PlayerId -> Dominion ()

-- | When you use a card (either you play it or you buy something),
-- you get a `PlayResult`. A `PlayResult` is either a `Left` with an error message,
-- or a `Right` with a value.
type PlayResult = Maybe String

-- | Each `PlayerResult` is a tuple of a player and their final score.
type PlayerResult = (Player, Int)

-- | Players and their scores.
data Result = Result {
            playerResults :: [PlayerResult],
            winner        :: String
            } deriving (Show)

-- | Convenience function. Prints out a line if verbose, AND prints out
-- info about the related player...name, money, # of buys, # of actions.
log :: PlayerId -> String -> Dominion ()
log playerId str = do
    player <- getPlayer playerId
    money <- handValue playerId
    let name = playerName player
        bs = buys player
        as = actions player
        statusLine = printf "[player %s, name: %s, money: %s, buys: %s, actions: %s]" (yellow . show $ playerId) (yellow name) (green . show $ money) (green . show $ bs) (red . show $ as)
    log_ $ statusLine ++ ": " ++ green str

-- | Like `log` but doesn't print out info about a player
log_ :: String -> Dominion ()
log_ str = do
    state <- get
    when (verbose state) $ liftIO . putStrLn $ str

-- | see all of the cards in a player's hand.
--
-- > cards <- currentHand playerId
currentHand :: PlayerId -> Dominion [CardWrap]
currentHand playerId = hand <$> getPlayer playerId

-- | see if a player has a card in his hand.
--
-- > hasCard <- playerId `has` chapel
has :: PlayerId -> CardWrap -> Dominion Bool
has playerId card = do
    player <- getPlayer playerId
    return $ card `elem` hand player

-- | see how many of this card a player has.
--
-- > numMarkets <- countNum playerId market
countNum :: PlayerId -> CardWrap -> Dominion Int
countNum playerId card = do
    player <- getPlayer playerId
    let allCards = deck player ++ discard player ++ hand player
    return $ count card allCards

-- | Get the current round number.
getRound :: Dominion Int
getRound = roundNum <$> get

-- | How much money this player's hand is worth (also counts any money you
-- get from action cards, like +1 from market).
handValue :: PlayerId -> Dominion Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (hand player)) + extraMoney player

-- | Check if this card's pile is empty. Returns True is the card is not in play.
pileEmpty :: CardWrap -> Dominion Bool
pileEmpty card = do
    state <- get
    return $ case M.lookup card (cards state) of
      Nothing -> True
      Just x -> x == 0

-- | Returns the card, or Nothing if that pile is empty.
-- Useful because it automatically checks whether the pile is empty, and
-- modifies state to subtract a card from the pile correctly.
getCard :: CardWrap -> Dominion (Maybe CardWrap)
getCard card = do
    empty <- pileEmpty card
    if empty
      then return Nothing
      else do
        modify $ \s -> s{cards=decrement card (cards s)}
        return $ Just card

-- | Given a player id and a number of cards to draw, draws that many cards
-- from the deck, shuffling if necessary.
drawFromDeck :: PlayerId -> Int -> Dominion [CardWrap]
drawFromDeck playerId numCards = do
    player <- getPlayer playerId
    let dk = deck player
    if length dk >= numCards
      then draw numCards
      else do
        let inDeck = length dk
        lastCards <- draw inDeck
        shuffleDeck playerId
        liftM (++ lastCards) $ draw (numCards - inDeck)
 where
   draw numCards = do
       player <- getPlayer playerId
       let drawnCards = take numCards (deck player)
       modifyPlayer playerId (\p -> p{deck=drop numCards (deck p),hand=hand p ++ drawnCards})
       return drawnCards

-- | Like `modify` for the `State` monad, but works on players.
-- Takes a player id and a function that modifies the player.
modifyPlayer :: PlayerId -> (Player -> Player) -> Dominion ()
modifyPlayer playerId func = modify $ \s -> s{players=modifyListIndex playerId func (players s)}

-- | Like `modifyPlayer`, but modifies every player *except* the one specified with the player id.
modifyOtherPlayers :: PlayerId -> (Player -> Player) -> Dominion ()
modifyOtherPlayers playerId func = do
    state <- get
    let ps = indices (players state) \\ [playerId]
    forM_ ps $ \pid -> modifyPlayer pid func

otherPlayers :: PlayerId -> Dominion [PlayerId]
otherPlayers pid = do
    state <- get
    return $ indices (players state) \\ [pid]

setupForTurn :: PlayerId -> Dominion ()
setupForTurn playerId = do
    drawFromDeck playerId 5
    modifyPlayer playerId (\s -> s{actions=1,buys=1,extraMoney=0})

playTurn :: PlayerId -> Strategy -> Dominion ()
playTurn playerId strategy = do
    roundNum <- getRound
    when (roundNum == 1) $ setupForTurn playerId
    player <- getPlayer playerId
    log playerId $ "player's hand has: " ++ show (hand player)
    strategy playerId
    discardHand playerId
    -- we draw from deck *after* to set up the next hand NOW,
    -- instead of calling this at the beginning of the function.
    -- The reason is, if someone else plays a militia, or a council room,
    -- these players need to be able to modify their deck accordingly
    -- even if its not their turn.
    setupForTurn playerId

isAction card = Action `elem` types card
isAttack card = Attack `elem` types card
isReaction card = Reaction `elem` types card
isTreasure card = Treasure `elem` types card
isVictory card = Victory `elem` types card

countPoints :: PlayerId -> Dominion Int
countPoints playerId = do
  player <- getPlayer playerId
  let cards = concat $ [deck, discard, hand] <*> pure player
  foldM (\accum card -> do x <- points card playerId
                           return (x + accum)) 0 cards

collectStats :: Dominion GameStats
collectStats = do
  state <- get
  let ids = indices $ players state
  vps <- mapM (\x -> do points <- countPoints x
                        return (x, points)) ids
  return GameStats{victoryPoints=M.fromList vps}

-- | Get player from game state specified by this id.
-- This is useful sometimes:
--
-- > import qualified Dominion.Types as T
-- > import Control.Lens
-- >
-- > player <- getPlayer playerId
-- >
-- > -- How many buys does this player have?
-- > player ^. buys
-- >
-- > -- How many actions does this player have?
-- > player ^. actions
getPlayer :: PlayerId -> Dominion Player
getPlayer playerId = do
    state <- get
    return $ players state !! playerId

-- | Convenience function. @ 4 \`cardsOf\` estate @ is the same as @ take 4 . repeat $ estate @
cardsOf = replicate

-- | Move this players discards + hand into his deck and shuffle the deck.
shuffleDeck :: PlayerId -> Dominion ()
shuffleDeck playerId = do player <- getPlayer playerId
                          shuffleFunction <- liftIO $ shuffleDeck_ player
                          modifyPlayer playerId shuffleFunction

shuffleDeck_ :: Player -> IO (Player -> Player)
shuffleDeck_ player = do newDeck <- deckShuffle . concat $ [deck, discard, hand] <*> pure player
                         return $ const player{discard=[], deck=newDeck}

-- | Check that this player is able to purchase this card. Returns
-- a `Right` if they can purchase the card, otherwise returns a `Left` with
-- the reason why they can't purchase it.
validateBuy :: PlayerId -> CardWrap -> Dominion PlayResult
validateBuy playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    cardGone <- pileEmpty card
    return . getFirst . mconcat . map First $
      [failIf (money < cost card) $ printf "Not enough money. You have %d but this card costs %d" money (cost card)
      ,failIf cardGone $ printf "We've run out of that card (%s)" (name card)
      ,failIf (buys player < 1) "You don't have any buys remaining!"
      ]

-- | Check that this player is able to play this card. Returns
-- a `Right` if they can play the card, otherwise returns a `Left` with
-- the reason why they can't play it.
validatePlay :: PlayerId -> CardWrap -> Dominion PlayResult
validatePlay playerId card = do
    player <- getPlayer playerId
    return . getFirst . mconcat . map First $
      [failIf (actions player < 1) "You don't have any actions remaining!"
      ,failIf (card `notElem` hand player) $ printf
        "You can't play a %s because you don't have it in your hand!" (name card)
      ]

-- Discard this player's hand.
discardHand :: PlayerId -> Dominion ()
discardHand playerId = modifyPlayer playerId $ \player -> player{hand=[],discard=discard player ++ hand player}

-- | Keep drawing a card until the provided function returns true.
-- The function gets a list of the cards drawn so far,
-- most recent first. Returns a list of all the cards drawn (these cards
-- are also placed into the player's hand)
drawsUntil :: PlayerId -> ([CardWrap] -> Dominion Bool) -> Dominion [CardWrap]
drawsUntil = drawsUntil_ []

-- internal use for drawsUntil
drawsUntil_ :: [CardWrap] -> PlayerId -> ([CardWrap] -> Dominion Bool) -> Dominion [CardWrap]
drawsUntil_ alreadyDrawn playerId func = do
    drawnCards <- drawFromDeck playerId 1
    let cards = drawnCards ++ alreadyDrawn
    stopDrawing <- func cards
    if stopDrawing
      then return cards
      else drawsUntil_ cards playerId func

-- | Player trashes the given card.
trashesCard :: PlayerId -> CardWrap -> Dominion ()
playerId `trashesCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{hand=delete card (hand p)})

-- | Player discards the given card.
discardsCard :: PlayerId -> CardWrap -> Dominion Bool
playerId `discardsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{hand=delete card (hand p),discard=card:discard p})
  return hasCard

-- | Player discards all given cards. Returns the cards that were discarded
discardsAll :: PlayerId -> [CardWrap] -> Dominion [CardWrap]
discardsAll pid [] = return []
discardsAll pid (card:cards) = do
  had <- pid `discardsCard` card
  rest <- pid `discardsAll` cards
  return $ if had then card:rest else rest

-- Player returns the given card to the top of their deck.
returnsCard :: PlayerId -> CardWrap -> Dominion ()
playerId `returnsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{hand=delete card (hand p), deck=card:deck p})

-- If the top card in the player's deck is one of the cards
-- listed in the provided array, then discard that card (used with spy).
discardTopCard :: [CardWrap] -> Player -> Player
discardTopCard cards player = if topCard `elem` cards
                                then player{deck=tail dck,discard=topCard:discard player}
                                else player
    where dck@(topCard:_) = deck player

-- TODO reaction
-- If this player has a victory card in his/her hand,
-- it is put on top of their deck *unless* they have a moat in their hand.
-- Used with militia.
returnVPCard :: Player -> Player
returnVPCard player = let hnd = hand player
                          victoryCards = filter isVictory hnd
                          card = head victoryCards
                      in player{hand=delete card hnd,deck=card:deck player}

-- TODO how do they choose what to discard??
-- Right now I'm just choosing to discard the least expensive.
-- | Player discards down to x cards.
discardsTo :: Player -> Int -> Player
player `discardsTo` x = player{hand=toKeep,discard=discard player ++ toDiscard}
    where hnd = sortBy (comparing cost) $ hand player
          toDiscard = take (length hnd - x) hnd
          toKeep = hnd \\ toDiscard

-- Checks that the player can gain the given card, then adds it to his/her
-- discard pile.
gainCardUpTo :: PlayerId -> Int -> CardWrap -> Dominion PlayResult
gainCardUpTo playerId value card =
  if cost card > value
    then return . Just $ printf "Card is too expensive. You can gain a card costing up to %d but this card costs %d" value (cost card)
    else do
      result <- getCard card
      case result of
        Nothing -> return . Just $ printf "We've run out of that card (%s)" (name card)
        (Just card_) -> do
          modifyPlayer playerId (\p -> p{discard=card : discard p})
          return Nothing

---------------------
-- Basic Card Actions
---------------------

noop :: Dominion PlayResult
noop = mempty

vpValue :: Int -> PlayerId -> Dominion Int
vpValue n _ = return n

validator :: CardWrap -> PlayerId -> Dominion PlayResult
validator card pid = do result <- validatePlay pid card
                        case result of
                          Just x -> return (Just x)
                          Nothing -> return Nothing

mkAction :: (PlayerId -> Dominion a) -> PlayerId -> Dominion PlayResult
mkAction action pid = do action pid
                         return Nothing

trasher :: CardWrap -> PlayerId -> Dominion PlayResult
trasher card = mkAction $ \pid -> do log pid ("trashes a " ++ show card)
                                     trashesCard pid card

discarder :: CardWrap -> PlayerId -> Dominion PlayResult
discarder card = mkAction $ \pid -> discardsCard pid card

plusCards :: Int -> PlayerId -> Dominion PlayResult
plusCards n = mkAction $ \pid -> do log pid ("+ " ++ show n ++ " cards")
                                    cards <- drawFromDeck pid n
                                    log pid ("drew " ++ show cards)

plusActions :: Int -> PlayerId -> Dominion PlayResult
plusActions n = mkAction $ \pid -> do log pid ("+ " ++ show n ++ " actions")
                                      modifyPlayer pid $ \p -> p{actions=actions p + n}

plusCoins :: Int -> PlayerId -> Dominion PlayResult
plusCoins n = mkAction $ \pid -> do log pid ("+ " ++ show n ++ " coins")
                                    modifyPlayer pid $ \p -> p{extraMoney=extraMoney p + n}

plusBuys :: Int -> PlayerId -> Dominion PlayResult
plusBuys n = mkAction $ \pid -> do log pid ("+ " ++ show n ++ " buys")
                                   modifyPlayer pid $ \p -> p{buys=buys p + n}

attack :: PlayerId -> (PlayerId -> Dominion a) -> Dominion [(PlayerId, a)]
attack player f = do players <- otherPlayers player
                     forM players $ \pid -> do result <- f pid
                                               return (pid, result)

mkCard :: Card a => a -> CardWrap
mkCard = CardWrap
