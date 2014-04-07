module Dominion.Cards
    ( module Dominion.Cards.Base
    , module Dominion.Cards.Original
    , allCards
    , allActionCards
    )

where

import Dominion.Types
import Dominion.Cards.Base
import Dominion.Cards.Original

allActionCards :: CardList
allActionCards = []

allCards :: CardList
allCards = treasureCards ++ victoryCards ++ allActionCards