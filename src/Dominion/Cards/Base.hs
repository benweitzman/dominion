{-# LANGUAGE MultiParamTypeClasses #-}

module Dominion.Cards.Base where

import Dominion.Card
import Dominion.Types

data Copper = Copper
instance Card Copper () where
    cardInfo _ = defaultCardInfo
               { getName = "Copper"
               , getCost = 0
               , getValue = 1
               , getTypes = [Treasure]
               }
    cardEffect _ = \_ -> return ()
copper = mkCard Copper

data Silver = Silver
instance Card Silver () where
    cardInfo _ = defaultCardInfo
               { getName = "Silver"
               , getCost = 3
               , getValue = 2
               , getTypes = [Treasure]
               }
    cardEffect _ = \_ -> return ()
silver = mkCard Silver

data Gold = Gold
instance Card Gold () where
    cardInfo _ = defaultCardInfo
               { getName = "Gold"
               , getCost = 6
               , getValue = 3
               , getTypes = [Treasure]
               }
    cardEffect _ = \_ -> return ()
gold = mkCard Gold

data Estate = Estate
instance Card Estate () where
    cardInfo _ = defaultCardInfo
               { getName = "Estate"
               , getCost = 2
               , getValue = 0
               , getTypes = [Victory]
               , getPoints = vpValue 1
               }
    cardEffect _ = \_ -> return ()
estate = mkCard Estate

data Duchy = Duchy
instance Card Duchy () where
    cardInfo _ = defaultCardInfo
               { getName = "Duchy"
               , getCost = 5
               , getTypes = [Victory]
               , getPoints = vpValue 3
               }
    cardEffect _ = \_ -> return ()
duchy = mkCard Duchy

data Province = Province
instance Card Province () where
    cardInfo _ = defaultCardInfo
               { getName = "Province"
               , getCost = 8
               , getTypes = [Victory]
               , getPoints = vpValue 6
               }
    cardEffect _ = \_ -> return ()
province = mkCard Province

data Curse = Curse
instance Card Curse () where
    cardInfo _ = defaultCardInfo
               { getName = "Curse"
               , getCost = 0
               , getTypes = [Victory]
               , getPoints = vpValue (-1)
               }
    cardEffect _ = \_ -> return ()
curse = mkCard Curse

treasureCards :: CardList
treasureCards = [copper, silver, gold]

victoryCards :: CardList
victoryCards = [estate, duchy, province, curse]