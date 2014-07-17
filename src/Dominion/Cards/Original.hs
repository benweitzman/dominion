{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Dominion.Cards.Original where

import Control.Applicative
import Control.Monad

import Dominion.Card
import Dominion.VirtualCard
import Dominion.Types
import Dominion.Internal
import Dominion.Player

{- TODO Adventurer
Action  $6  
Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards 
in your hand and discard the other revealed cards.
-}

{- TODO Bureaucrat
Action/Attack $4
Gain a silver card; put it on top of your deck. Each other player reveals a Victory card 
from his hand and puts it on his deck (or reveals a hand with no Victory cards).
-}

data Cellar = Cellar
instance Card Cellar Int where
  cardInfo _ = defaultCardInfo
             { getName = "Cellar"
             , getCost = 2
             , getTypes = [Action]
             }
  cardEffect _ = plusActions 1 

instance Effectful Cellar CardList CardList where
  Cellar `with` cards = Virtual (\pid -> do f pid
                                            discarded <- pid `discardsAll` cards
                                            drawFromDeck pid $ length discarded)
                                (s >> t)
                                (\_ -> return ())
    where (Virtual f s t) = toVirtual cellar
cellar = mkCard Cellar

data Chapel = Chapel
instance Card Chapel () where
  cardInfo _ = defaultCardInfo
             { getName = "Chapel"
             , getCost = 2
             , getTypes = [Action]
             }
  cardEffect _ _ = return ()

instance Effectful Chapel CardList CardList where
  Chapel `with` cards = Virtual (\pid -> (pid `trashesNByPreference` 4) cards)
                                (s >> t)
                                (\_ -> return ())
    where (Virtual _ s t) = toVirtual chapel
chapel = mkCard Chapel  

data Chancellor = Chancellor
instance Card Chancellor Int where
  cardInfo _ = defaultCardInfo
             { getName = "Chancellor"
             , getCost = 3
             , getTypes = [Action]
             }
  cardEffect _ = plusMoney 2

data ChancellorChoice = DiscardDeck | KeepDeck

instance Effectful Chancellor ChancellorChoice Int where
  Chancellor `with` KeepDeck = toVirtual chancellor
  Chancellor `with` DiscardDeck = Virtual (\pid -> do r <- f pid
                                                      p <- getPlayer pid
                                                      modifyPlayer pid $ modifyDiscard (deck p ++) . modifyDeck (const [])
                                                      return r)
                                   s
                                   t
    where (Virtual f s t) = toVirtual chancellor
chancellor = mkCard Chancellor

{- TODO Council Room 
Action $5
+4 Cards, + 1 Buy
Each other player draws a card
-}

{- TODO Feast
Action $4
Trash this card, gain a card up to $5
-}

data Festival = Festival
instance Card Festival () where
    cardInfo _ = defaultCardInfo
               { getName   = "Festival"
               , getCost   = 5
               , getTypes  = [Action]
               }
    cardEffect _ = \pid -> void $ plusActions 2 pid >> plusBuys 1 pid >> plusMoney 2 pid
festival = mkCard Festival

data Gardens = Gardens
instance Card Gardens () where
  cardInfo _ = defaultCardInfo
             { getName = "Gardens"
             , getCost = 4
             , getTypes = [Victory]
             , getPoints = \pid -> do player <- getPlayer pid
                                      let cards = concat $ [deck, discard, hand] <*> pure player
                                      return ((length cards `div` 10) * 4)
             }
  cardEffect _ _ = return ()
gardens = mkCard Gardens             

data Laboratory = Laboratory
instance Card Laboratory CardList where
  cardInfo _ = defaultCardInfo
             { getName = "Laboratory"
             , getCost = 5
             , getTypes = [Action]
             }
  cardEffect _ = plusActions 1 >> plusCards 2

{- TODO Library 
Action $5
draw until you have seven cards in your hand. you may set aside any action cards drawn this way, as you draw them.
discard the set aside cards after you finish drawing.
-}

data Market = Market
instance Card Market CardList where
  cardInfo _ = defaultCardInfo
             { getName = "Market"
             , getCost = 5
             , getTypes = [Action]
             }
  cardEffect _ = plusActions 1 >> plusBuys 1 >> plusMoney 1 >> plusCards 1             

{- TODO Militia
Action/Attack $4
+$2, each other player discards down to 3 cards
-}

{- TODO Mine
Action $5
Trash a treasure card from your hand. gain a treasure card costing up to $3 more
-}

{- TODO Moat 
Action/Reaction $2
+2 cards. When another player plays an attack card you may reveal this from your hand. If you do,
you are unaffected by the attack.
-}

{- TODO Moneylender 
Action $4
Trash a copper from your hand. If you do +$3
-}

{- TODO Remodel
Action $4
Trash a card from your hand. gain a card costing up to $2 more than the trashed card
-}

data Smithy = Smithy
instance Card Smithy CardList where
    cardInfo _ = defaultCardInfo
               { getName   = "Smithy"
               , getCost   = 4
               , getTypes  = [Action]
               }
    cardEffect _ = plusCards 3
smithy = mkCard Smithy

{- TODO Spy
Action/Attack $4
+1 card, +1 action
each player, including you, reveals that top card of his or her deck and either discards it or puts it back, 
your choice
-}

{- TODO Thief
Action/Attack $4
Each other plaer reveals the top 2 cards of his or her deck. If they revealed any Treasure cards, they trash
one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards
-}

data ThroneRoom = ThroneRoom
instance Card ThroneRoom () where
    cardInfo _ = defaultCardInfo 
               { getName = "ThroneRoom"
               , getCost = 4
               , getTypes = [Action] 
               }
    cardEffect _ _ = return ()

instance Virtualizable a b => Effectful ThroneRoom a (b, b) where
    tr `with` playable = Virtual (\pid -> do r1 <- f pid
                                             r2 <- f pid
                                             return (r1, r2))
                                 (\pid -> pid `plays` throneRoom >> s pid)
                                 (\pid -> pid `discardsCard` throneRoom >> t pid)
        where (Virtual f s t) = toVirtual playable

instance Effectful ThroneRoom ThroneRoom (ThroneRoom, ThroneRoom) where
    tr1 `with` tr2 = Virtual (\_ -> return (ThroneRoom, ThroneRoom))
                             (\pid -> pid `plays` throneRoom >> void (pid `plays` throneRoom))
                             (\pid -> pid `discardsCard` throneRoom >> void (pid `discardsCard` throneRoom))

instance (Virtualizable a b, Virtualizable c d) => Effectful (Virtual (ThroneRoom, ThroneRoom)) (a, c) ((b, b), (d, d)) where
    vtr `with` (p1, p2) = Virtual (\pid -> do r1 <- f1 pid
                                              r2 <- f1 pid
                                              r3 <- f2 pid
                                              r4 <- f2 pid
                                              return ((r1, r2), (r3, r4)))
                                  (\pid -> ts pid >> s1 pid >> s2 pid)
                                  (\pid -> tt pid >> t1 pid >> t2 pid)
        where (Virtual f1 s1 t1) = toVirtual p1
              (Virtual f2 s2 t2) = toVirtual p2
              (Virtual _ ts tt) = vtr
throneRoom = mkCard ThroneRoom

data Village = Village
instance Card Village [VirtualCard ()] where
    cardInfo _ = defaultCardInfo
               { getName = "Village"
               , getCost = 3
               , getTypes = [Action]
               }
    cardEffect _ = \pid -> plusActions 2 pid >> plusCards 1 pid
village = mkCard Village

{- TODO With 
Action/Attack $5
+2 cards, each other player gains a curse card
-}

data Woodcutter = Woodcutter
instance Card Woodcutter () where
  cardInfo _ = defaultCardInfo
             { getName = "Woodcutter"
             , getCost = 3
             , getTypes = [Action]
             }
  cardEffect _ = \pid -> void $ plusBuys 1 pid >> plusMoney 2 pid
woodcutter = mkCard Woodcutter

data Workshop = Workshop
instance Card Workshop () where
  cardInfo _ = defaultCardInfo
             { getName = "Workshop"
             , getCost = 3
             , getTypes = [Action]
             }
  cardEffect _ _ = return ()
workshop = mkCard Workshop