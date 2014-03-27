module Dominion.Strategies where
import Dominion
import Dominion.Cards
import qualified Dominion.Types as T
import Control.Monad

-- | Buy the most expensive victory or treasure card you can.
bigMoney playerId = playerId `buysByPreference` [province, gold, duchy, silver, copper]

-- | Same as `bigMoney` but also buy a `smithy` whenever you can.
bigMoneySmithy playerId = do
    playerId `plays` smithy
    roundNum <- getRound
    if (roundNum < 6)
        then playerId `buysByPreference` [province, gold, smithy, silver]
        else playerId `buysByPreference` [province, gold, duchy, silver, estate]

bigMoney2 playerId = do 
    roundNum <- getRound
    if (roundNum < 6)
        then playerId `buysByPreference` [province, gold, silver]
        else playerId `buysByPreference` [province, gold, duchy, silver, estate]

villageSmithy playerId = do
    playerId `playsByPreference` [village, smithy]
    roundNum <- getRound
    if (roundNum < 6)
        then playerId `buysByPreference` [province, gold, smithy, village, silver]
        else playerId `buysByPreference` [province, gold, duchy, silver, estate]

throneRoomSmithy playerId = do
    playerId `plays` (ThroneRoom `T.with` (ThroneRoom `T.with` smithy))
    playerId `plays` (ThroneRoom `T.with` smithy)
    playerId `plays` smithy
    roundNum <- getRound
    if (roundNum < 6)
        then playerId `buysByPreference` [province, gold, smithy, silver]
        else playerId `buysByPreference` [province, gold, duchy, throneroom, silver, estate]      

-- | A strategy that should never win: buy only provinces and golds
-- exclusively.
stupidStrategy playerId = playerId `buysByPreference` [province, silver]

justEstates playerId = playerId `buysByPreference` [estate]
