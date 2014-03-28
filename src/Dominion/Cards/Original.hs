{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Dominion.Cards.Original (
  -- | Playing a card is easy:
  --
  -- > playerId `plays` adventurer
  module Dominion.Cards.Original                             
) where

import Control.Monad.State hiding (state)
import Dominion.Cards.Base
import Dominion.Internal
import Data.List
import Data.Monoid

{-

-- | 
-- > playerId `plays` chapel `with` (Chapel [list of cards to trash])
chapel      = Card "Chapel" 2 [Action] [TrashCards 4]
councilRoom = Card "Council Room" 5 [Action] [PlusCard 4, PlusBuy 1, OthersPlusCard 1]

-- | To gain a market, for example:
--
-- > playerId `plays` feast `with` (Feast market)
laboratory  = Card "Laboratory" 5 [Action] [PlusCard 2, PlusAction 1]
library     = Card "Library" 5 [Action] [LibrarygetEffect]
militia     = Card "Militia" 4 [Action, Attack] [PlusCoin 2, OthersDiscardTo 3]

-- | 
-- > playerId `plays` mine `with` (Mine copper)
mine        = Card "Mine" 5 [Action] [MinegetEffect]
moneylender = Card "Moneylender" 4 [Action] [MoneylendergetEffect]

-- | To turn a gold into a province:
--
-- > playerId `plays` remodel `with` (Remodel (gold, province))
remodel     = Card "Remodel" 4 [Action] [RemodelgetEffect]

-- | The `Spy` `FollowupAction` takes two lists: a list of cards you would
-- discard for yourself, and a list of cards you would discard for others:
--
-- > playerId `plays` spy `with` ([estate, duchy, province], [silver, gold])
spy         = Card "Spy" 4 [Action, Attack] [PlusCard 1, PlusAction 1, SpygetEffect]

                    -- gets a list of the treasure cards that the player
                    -- had. You return either TrashOnly to have the player
                    -- trash a card, or GainTrashedCard to gain the trashed
                    -- card.

-- | You need to provide a function that takes a list of treasure cards and
-- picks one to trash. You can either return a `TrashOnly` to trash the
-- card, or a `GainTrashedCard` to put it into your discard pile.
--
-- > playerId `plays` thief `with` (Thief (GainTrashedCard . sortBy (comparing coinValue)))
thief       = Card "Thief" 4 [Action, Attack] [ThiefgetEffect]

witch       = Card "Witch" 5 [Action, Attack] [PlusCard 2, OthersGainCurse 1]

-- | 
-- > playerId `plays` workshop `with` (Workshop gardens)
workshop    = Card "Workshop" 3 [Action] [GainCardUpto 4]
gardens     = Card "Gardens" 4 [Victory] [GardensgetEffect]
-}

data Adventurer = Adventurer
adventurer = mkCard Adventurer
instance Card Adventurer where
    name _ = "Adventurer"
    cost _ = 6
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator adventurer
    getEffect _ = \pid -> do drawnCards <- drawsUntil pid (\cards -> return $ length (filter isTreasure cards) == 2)
                             let (treasure, other) = partition isTreasure drawnCards
                             forM other (discardsCard pid)
                             return Nothing
    getTearDown _ = discarder adventurer

data BureaucratResult = Returned CardWrap | Revealed [CardWrap]

data Bureaucrat = Bureaucrat
bureaucrat = mkCard Bureaucrat
instance Card Bureaucrat where
    name _ = "Bureaucrat"
    cost _ = 4
    coinValue _ = 0
    types _ = [Action, Attack]
    getSetup _= validator bureaucrat
    getEffect _ = \pid -> do s <- getCard silver
                             case s of 
                               Nothing -> return ()
                               Just card -> modifyPlayer pid $ (\p -> p{deck=card:deck p})
                             attack pid $ \otherPlayer -> do h <- currentHand otherPlayer
                                                             let victoryCards = filter isVictory h
                                                             if (null victoryCards) 
                                                                then return $ Revealed h
                                                                else let c = head victoryCards in
                                                                      do returnsCard otherPlayer c
                                                                         return $ Returned c
                             return Nothing
    getTearDown _ = discarder bureaucrat

data Cellar = Cellar
cellar = mkCard Cellar
instance Card Cellar where
    name _ = "Cellar"
    cost _ = 2
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator cellar
    getEffect _ = plusActions 1
    getTearDown _ = discarder cellar

instance Effectful Cellar [CardWrap] where
    cellar `with` cards = Virtual { setupFunction = getSetup cellar
                                  , effectFunction = \pid -> getEffect cellar pid <> (pid `discardsAll` cards >> return Nothing)
                                  , tearDownFunction = getTearDown cellar
                                  }
                                  
data Chancellor = Chancellor
chancellor = mkCard Chancellor
instance Card Chancellor where
    name _ = "Chancellor"
    cost _ = 3
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator chancellor
    getEffect _ = plusCoins 2
    getTearDown _ = discarder chancellor

instance Effectful Chancellor Bool where
    chancellor `with` False = fromCard chancellor
    chancellor `with` True = Virtual { setupFunction = getSetup chancellor
                                     , effectFunction = \pid -> getEffect chancellor pid <> 
                                                                (modifyPlayer pid (\p -> p{deck=[],discard=deck p ++ discard p}) >> return Nothing)
                                     , tearDownFunction = getTearDown chancellor
                                     }

data Festival = Festival
festival = mkCard Festival
instance Card Festival where
    name _ = "Festival"
    cost _ = 5
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator festival
    getEffect _ = plusActions 2 <> plusCoins 2 <> plusBuys 1
    getTearDown _ = discarder festival

data Laboratory = Laboratory
laboratory = mkCard Laboratory
instance Card Laboratory where
    name _ = "Laboratory"
    cost _ = 5
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator laboratory
    getEffect _ = plusCards 2 <> plusActions 2
    getTearDown _ = discarder laboratory

data Market = Market
market = mkCard Market
instance Card Market where
    name _ = "Market"
    cost _ = 5
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator market
    getEffect _ = plusActions 1 <> plusCoins 1 <> plusCards 1 <> plusBuys 1
    getTearDown _ = discarder market

data Moat = Moat
moat = mkCard Moat
instance Card Moat where
    name _ = "Moat"
    cost _ = 2
    coinValue _ = 0
    types _ = [Action, Reaction]
    getSetup _= validator moat
    getEffect _ = plusCards 2
    getTearDown _ = discarder moat

data ThroneRoom = ThroneRoom
throneroom = mkCard ThroneRoom
instance Card ThroneRoom where
    name _ = "ThroneRoom"
    cost _ = 4
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator throneroom
    getTearDown _ = discarder throneroom

instance Playable a => Effectful ThroneRoom a where
    throneRoom `with` playable = Virtual { setupFunction = \pid -> do r1 <- getSetup throneRoom pid 
                                                                      case r1 of
                                                                        Nothing -> do state <- get
                                                                                      getTearDown throneRoom pid
                                                                                      r2 <- setup pid playable
                                                                                      put state
                                                                                      return r2
                                                                        x -> return x
                                         , effectFunction = \pid -> effect pid throneRoom <> effect pid playable <> effect pid playable
                                         , tearDownFunction = \pid -> getTearDown throneRoom pid <> tearDown pid playable
                                         }

data Smithy = Smithy 
smithy = mkCard Smithy
instance Card Smithy where
    name _ = "Smithy"
    cost _ = 4
    coinValue _ = 0
    types _ = [Action]
    getSetup _ = validator smithy
    getEffect _ = plusCards 3
    getTearDown _ = discarder smithy

data Village = Village
village = mkCard Village
instance Card Village where
    name _ = "Village"
    cost _ = 3
    coinValue _ = 3
    types _ = [Action]
    getSetup _= validator village
    getEffect _ = plusCards 1 <> plusActions 2
    getTearDown _ = discarder village

data Woodcutter = Woodcutter
woodcutter = mkCard Woodcutter
instance Card Woodcutter where
    name _ = "Woodcutter"
    cost _ = 3
    coinValue _ = 0
    types _ = [Action]
    getSetup _= validator woodcutter
    getEffect _ = plusCoins 1 <> plusBuys 1
    getTearDown _ = discarder woodcutter

originalCards = [smithy, throneroom, village, woodcutter]                
