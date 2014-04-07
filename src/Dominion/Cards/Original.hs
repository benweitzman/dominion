{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Dominion.Cards.Original where

import Dominion.Card
import Dominion.Internal
import Dominion.VirtualCard
import Dominion.Types

data Smithy = Smithy
instance Card Smithy [VirtualCard ()] where
    cardInfo _ = defaultCardInfo
               { getName   = "Smithy"
               , getCost   = 4
               , getTypes  = [Action]
               }
    cardEffect _ = plusCards 3
smithy = mkCard Smithy

{-


throneRoom :: Playable ()
throneRoom = card
    { name  = "ThroneRoom"
    , cost  = 4
    , types = [Action]
    , with  =  \playable -> Virtual $ \pid -> try $ do play pid throneRoom 
                                                       play pid playable
                                                       effect playable pid
    }

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