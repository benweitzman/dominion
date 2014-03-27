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
    setup _ = validator copper

data Silver = Silver
silver = mkCard Silver
instance Card Silver where
    name _ = "Silver"
    cost _ = 3
    coinValue _ = 2
    types _ = [Treasure]
    setup _ = validator silver

data Gold = Gold
gold = mkCard Gold
instance Card Gold where
    name _ = "Gold"
    cost _ = 6
    coinValue _ = 3
    types _ = [Treasure]
    setup _ = validator gold

data Estate = Estate
estate = mkCard Estate
instance Card Estate where
    name _ = "Estate"
    cost _ = 2
    coinValue _ = 0
    types _ = [Victory]
    effect _ = vpValue 1
    setup _ = validator estate

data Duchy = Duchy
duchy = mkCard Duchy
instance Card Duchy where
    name _ = "Duchy"
    cost _ = 5
    coinValue _ = 0
    types _ = [Victory]
    effect _ = vpValue 3
    setup _ = validator duchy

data Province = Province
province = mkCard Province
instance Card Province where
    name _ = "Province"
    cost _ = 8
    coinValue _ = 0
    types _ = [Victory]
    effect _ = vpValue 6
    setup _ = validator province

data Curse = Curse 
curse = mkCard Curse
instance Card Curse where
    name _ = "Curse"
    cost _ = 0
    coinValue _ = 0
    types _ = [Victory]
    effect _ = vpValue (-1)
    setup _ = validator curse


treasureCards = [copper, silver, gold]
victoryCards = [estate, duchy, province, curse]
