module Dominion.Cards.Base where
import Dominion.Types
import Dominion.Internal
import Data.Monoid

copper   = mkCard "Copper" 0 1 [Treasure] noop
silver   = mkCard "Silver" 3 2 [Treasure] noop
gold     = mkCard "Gold" 6 3 [Treasure] noop

estate   = mkCard "Estate" 2 0 [Victory] (vpValue 1)
duchy    = mkCard "Duchy" 5 0 [Victory] (vpValue 3)
province = mkCard "Province" 8 0 [Victory] (vpValue 6)
curse    = mkCard "Curse" 0 0 [Victory] (vpValue (-1))

treasureCards = [copper, silver, gold]
victoryCards = [estate, duchy, province, curse]
