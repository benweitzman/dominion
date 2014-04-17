{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Dominion.Cards.Original where

import Control.Monad

import Dominion.Card
import Dominion.VirtualCard
import Dominion.Types
import Dominion.Internal
import Dominion.Player

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

instance Effectful Chancellor Bool Int where
  Chancellor `with` False = toVirtual chancellor
  Chancellor `with` True = Virtual (\pid -> do r <- f pid
                                               p <- getPlayer pid
                                               modifyPlayer pid $ modifyDiscard (deck p ++) . modifyDeck (const [])
                                               return r)
                                   s
                                   t
    where (Virtual f s t) = toVirtual chancellor
chancellor = mkCard Chancellor


data Smithy = Smithy
instance Card Smithy CardList where
    cardInfo _ = defaultCardInfo
               { getName   = "Smithy"
               , getCost   = 4
               , getTypes  = [Action]
               }
    cardEffect _ = plusCards 3
smithy = mkCard Smithy

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