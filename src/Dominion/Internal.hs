{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Dominion.Internal where

import Data.List
import Data.Monoid
import Data.Ord
import Dominion.Types
import Dominion.Util
import Dominion.Player
import Dominion.VirtualCard
import Dominion.Virtual
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Logger
import qualified Data.Map as M
import Prelude hiding (log)
import Text.Printf
import qualified Data.Text as T

instance Virtualizable (Virtual a) a where
  toVirtual x = x

instance Virtualizable (VirtualCard a) a where
  toVirtual c@(VirtualCard i f s) 
    | trash i = Virtual (\pid -> do log pid $ printf "plays a %s" (name c)
                                    result <- f pid
                                    return result) s (\pid -> pid `trashesCard` c >> return ())
    | otherwise = Virtual (\pid -> do log pid $ printf "plays a %s" (name c)
                                      result <- f pid
                                      return result) s (\pid -> pid `discardsCard` c >> return ())

{-
instance Card a b => Virtualizable a b where
    toVirtual card 
      | trash card = Virtual $ \pid -> do result <- cardEffect card pid
                                          pid `trashesCard` 
-}


collectStats :: Dominion GameStats
collectStats = do
  state <- get
  let ids = indices $ players state
  vps <- mapM (\x -> do points <- countPoints x
                        return (x, points)) ids
  return GameStats{victoryPoints=M.fromList vps}

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
currentHand :: PlayerId -> Dominion CardList
currentHand playerId = hand <$> getPlayer playerId

-- | see if a player has a card in his hand.
--
-- > hasCard <- playerId `has` chapel
has :: PlayerId -> VirtualCard a -> Dominion Bool
has playerId card = do
    player <- getPlayer playerId
    return $ (void card) `elem` hand player

-- | see how many of this card a player has.
--
-- > numMarkets <- countNum playerId market
countNum :: PlayerId -> VirtualCard a -> Dominion Int
countNum playerId card = do
    player <- getPlayer playerId
    let allCards = deck player ++ discard player ++ hand player
    return $ count (void card) allCards

-- | Get the current round number.
getRound :: Dominion Int
getRound = roundNum <$> get

-- | How much money this player's hand is worth (also counts any money you
-- get from action cards, like +1 from market).
handValue :: PlayerId -> Dominion Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map value (hand player)) + money player

-- | Check if this card's pile is empty. Returns True is the card is not in play.
pileEmpty :: VirtualCard a -> Dominion Bool
pileEmpty card = do
    state <- get
    return $ case M.lookup (void card) (cards state) of
      Nothing -> True
      Just x -> x == 0    

-- | Returns the card, or Nothing if that pile is empty.
-- Useful because it automatically checks whether the pile is empty, and
-- modifies state to subtract a card from the pile correctly.
getCard :: VirtualCard a -> Dominion (VirtualCard a)
getCard card = do
    empty <- pileEmpty card
    when empty . throwError $ printf "We've run out of that card (%s)" (name card)
    modify $ \s -> s{cards=decrement (void card) (cards s)}
    return card

-- | Given a player id and a number of cards to draw, draws that many cards
-- from the deck, shuffling if necessary.
drawFromDeck :: PlayerId -> Int -> Dominion CardList
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
    drawn <- drawFromDeck playerId 5
    $(logDebug) (T.pack $ "drew " ++ show drawn)
    modifyPlayer playerId resetPlayer

playTurn :: PlayerId -> Strategy -> Dominion ()
playTurn playerId strategy = do
    $(logDebug) (T.pack $ show playerId)
    roundNum <- getRound
    when (roundNum == 1) $ setupForTurn playerId
    player <- getPlayer playerId
    log playerId $ "player's hand has: " ++ show (hand player)
    $(logDebug) "hello"
    result <- strategy playerId
    $(logDebug) (T.pack $ show result)
    discardHand playerId
    -- we draw from deck *after* to set up the next hand NOW,
    -- instead of calling this at the beginning of the function.
    -- The reason is, if someone else plays a militia, or a council room,
    -- these players need to be able to modify their deck accordingly
    -- even if its not their turn.
    setupForTurn playerId

isAction :: VirtualCard a -> Bool
isAction card = Action `elem` types card

isAttack :: VirtualCard a -> Bool
isAttack card = Attack `elem` types card

isReaction :: VirtualCard a -> Bool
isReaction card = Reaction `elem` types card

isTreasure :: VirtualCard a -> Bool
isTreasure card = Treasure `elem` types card

isVictory :: VirtualCard a -> Bool
isVictory card = Victory `elem` types card

countPoints :: PlayerId -> Dominion Int
countPoints playerId = do
  player <- getPlayer playerId
  let cards = concat $ [deck, discard, hand] <*> pure player
  foldM (\accum card -> do x <- points card playerId
                           return (x + accum)) 0 cards

{-

collectStats :: Dominion GameStats
collectStats = do
  state <- get
  let ids = indices $ players state
  vps <- mapM (\x -> do points <- countPoints x
                        return (x, points)) ids
  return GameStats{victoryPoints=M.fromList vps}

-}

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


-- TODO replace with try
-- | Check that this player is able to purchase this card. Returns
-- a `Right` if they can purchase the card, otherwise returns a `Left` with
-- the reason why they can't purchase it.
validateBuy :: PlayerId -> VirtualCard a -> Dominion ()
validateBuy playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    cardGone <- pileEmpty card
    when (money < cost card) . throwError $ printf "Not enough money. You have %d but this card costs %d" money (cost card)
    when  cardGone . throwError $ printf "We've run out of that card (%s)" (name card)
    when (buys player < 1) $ throwError "You don't have any buys remaining!"

-- | Check that this player is able to play this card. Returns
-- a `Right` if they can play the card, otherwise returns a `Left` with
-- the reason why they can't play it.
validatePlay :: PlayerId -> VirtualCard a -> Dominion ()
validatePlay playerId card = do
    player <- getPlayer playerId
    when (actions player < 1) $ throwError "You don't have any actions remaining!"
    when ((void card) `notElem` hand player) . throwError $ printf
        "You can't play a %s because you don't have it in your hand!" (name card)


-- Discard this player's hand.
discardHand :: PlayerId -> Dominion ()
discardHand playerId = modifyPlayer playerId $ \player -> player{hand=[],discard=discard player ++ hand player}

-- | Keep drawing a card until the provided function returns true.
-- The function gets a list of the cards drawn so far,
-- most recent first. Returns a list of all the cards drawn (these cards
-- are also placed into the player's hand)
drawsUntil :: PlayerId -> (CardList -> Dominion Bool) -> Dominion CardList
drawsUntil = drawsUntil_ []

-- internal use for drawsUntil
drawsUntil_ :: CardList -> PlayerId -> (CardList -> Dominion Bool) -> Dominion CardList
drawsUntil_ alreadyDrawn playerId func = do
    drawnCards <- drawFromDeck playerId 1
    let cards = drawnCards ++ alreadyDrawn
    stopDrawing <- func cards
    if stopDrawing
      then return cards
      else drawsUntil_ cards playerId func

-- | Player trashes the given card.
trashesCard :: PlayerId -> VirtualCard a -> Dominion Bool
playerId `trashesCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{hand=delete (void card) (hand p)})
  return hasCard

trashesNByPreference :: PlayerId -> Int -> CardList -> Dominion CardList
trashesNByPreference _ 0 _ = return []
trashesNByPreference _ _ [] = return []
trashesNByPreference pid n (x:xs) = do hadCard <- trashesCard pid x
                                       if hadCard
                                        then do rest <- trashesNByPreference pid (n-1) (x:xs)
                                                return $ x:rest
                                        else trashesNByPreference pid n xs

-- | Player discards the given card.
discardsCard :: PlayerId -> VirtualCard a -> Dominion Bool
playerId `discardsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{hand=delete (void card) (hand p),discard=(void card):discard p})
  return hasCard

-- | Player discards all given cards. Returns the cards that were discarded
discardsAll :: PlayerId -> CardList -> Dominion CardList
discardsAll pid [] = return []
discardsAll pid (card:cards) = do
  had <- pid `discardsCard` card
  rest <- pid `discardsAll` cards
  return $ if had then card:rest else rest

-- Player returns the given card to the top of their deck.
returnsCard :: PlayerId -> VirtualCard a -> Dominion ()
playerId `returnsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{hand=delete (void card) (hand p), deck=(void card):deck p})

-- If the top card in the player's deck is one of the cards
-- listed in the provided array, then discard that card (used with spy).
discardTopCard :: CardList -> Player -> Player
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
gainCardUpTo :: PlayerId -> Int -> VirtualCard a -> Dominion (VirtualCard a)
gainCardUpTo playerId value card = do
  when (cost card > value) . throwError $ printf "Card is too expensive. You can gain a card costing up to %d but this card costs %d" value (cost card)
  result <- getCard card
  modifyPlayer playerId (\p -> p{discard=(void card) : discard p})
  return result

plays :: Virtualizable a b => PlayerId -> a -> Dominion b
plays pid v = do s pid 
                 $(logDebug) "play"
                 r <- f pid 
                 t pid
                 return r
  where (Virtual f s t) = toVirtual v

try :: Dominion a -> Dominion ()
try block = do state <- get
               block >> return () `catchError` \_ -> put state

tryCatch :: Dominion a -> (String -> Dominion a) -> Dominion a
tryCatch block handler = do state <- get
                            block `catchError` \e -> put state >> (handler e)
