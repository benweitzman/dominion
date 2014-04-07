module Dominion.Strategies where
import Dominion
import Dominion.Cards
import Dominion.Card
import Control.Monad

-- | Buy the most expensive victory or treasure card you can.
bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]

-- | Same as `bigMoney` but also buy a `smithy` whenever you can.
{- bigMoneySmithy playerId = do
    playerId `playsCard` smithy
    roundNum <- getRound
    if roundNum < 6
        then playerId `buysByPreference` [province, gold, (void smithy), silver]
        else playerId `buysByPreference` [province, gold, duchy, silver, estate]

bigMoney2 playerId = do 
    roundNum <- getRound
    if roundNum < 6
        then playerId `buysByPreference` [province, gold, silver]
        else playerId `buysByPreference` [province, gold, duchy, silver, estate]

villageSmithy playerId = do
    playerId `playsByPreference` [(void village), (void smithy)]
    roundNum <- getRound
    if roundNum < 6
        then playerId `buysByPreference` [province, gold, (void smithy), (void village), silver]
        else playerId `buysByPreference` [province, gold, duchy, silver, estate]

throneRoomSmithy playerId = do
    playerId `playsCard` (throneRoom `with` (void smithy))
    playerId `playsCard` smithy
    roundNum <- getRound
    if roundNum < 6
        then playerId `buysByPreference` [province, gold, (void smithy), silver]
        else playerId `buysByPreference` [province, gold, duchy, (void throneRoom), silver, estate]      -}

-- | A strategy that should never win: buy only provinces and golds
-- exclusively.
stupidStrategy playerId = playerId `buysByPreference` [province, silver]

justEstates playerId = playerId `buysByPreference` [estate]
