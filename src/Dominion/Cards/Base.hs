module Dominion.Cards.Base where
import Dominion.Types
import Dominion.Internal
import Data.Monoid

{- 
copper   = mkCard "Copper" 0 1 [Treasure] noop
silver   = mkCard "Silver" 3 2 [Treasure] noop
gold     = mkCard "Gold" 6 3 [Treasure] noop

estate   = mkCard "Estate" 2 0 [Victory] (vpValue 1)
duchy    = mkCard "Duchy" 5 0 [Victory] (vpValue 3)
province = mkCard "Province" 8 0 [Victory] (vpValue 6)
curse    = mkCard "Curse" 0 0 [Victory] (vpValue (-1))
-}
data Copper = Copper
copper = mkCard Copper
instance Card Copper where
    name _ = "Copper"
    cost _ = 0
    coinValue _ = 1
    types _ = [Treasure]

data Silver = Silver
silver = mkCard Silver
instance Card Silver where
    name _ = "Silver"
    cost _ = 3
    coinValue _ = 2
    types _ = [Treasure] 

data Gold = Gold
gold = mkCard Gold
instance Card Gold where
    name _ = "Gold"
    cost _ = 6
    coinValue _ = 3
    types _ = [Treasure]

data Estate = Estate
estate = mkCard Estate
instance Card Estate where
    name _ = "Estate"
    cost _ = 2
    coinValue _ = 0
    types _ = [Victory]
    points _ = vpValue 1

data Duchy = Duchy
duchy = mkCard Duchy
instance Card Duchy where
    name _ = "Duchy"
    cost _ = 5
    coinValue _ = 0
    types _ = [Victory]
    points _ = vpValue 3

data Province = Province
province = mkCard Province
instance Card Province where
    name _ = "Province"
    cost _ = 8
    coinValue _ = 0
    types _ = [Victory]
    points _ = vpValue 6

data Curse = Curse 
curse = mkCard Curse
instance Card Curse where
    name _ = "Curse"
    cost _ = 0
    coinValue _ = 0
    types _ = [Victory]
    points _ = vpValue (-1)

treasureCards = [copper, silver, gold]
victoryCards = [estate, duchy, province, curse]
