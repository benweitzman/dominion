module Dominion.Cards.Base where
import Dominion.Types
import Dominion.Internal

copper   = Card "Copper" 0 1 [Treasure] noop
silver   = Card "Silver" 3 2 [Treasure] noop
gold     = Card "Gold" 6 3 [Treasure] noop

estate   = Card "Estate" 2 0 [Victory] (vpValue 1)
duchy    = Card "Duchy" 5 0 [Victory] (vpValue 3)
province = Card "Province" 8 0 [Victory] (vpValue 6)
curse    = Card "Curse" 0 0 [Victory] (vpValue (-1))

treasureCards = [copper, silver, gold]
victoryCards = [estate, duchy, province, curse]
