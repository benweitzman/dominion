module Dominion.Card
    ( vpValue
    , CardType (..)
    , CardList
    , CardInfo (..)
    , noop
    , defaultCardInfo
    , plusCards
    , plusActions
    , mkCard
    )

where

import Data.Ord
import Dominion.Types
import Dominion.Internal
import Dominion.Player
import Prelude hiding (log)

--------------------------
-- CARD DEFINITION HELPERS
--------------------------
defaultCardInfo :: CardInfo
defaultCardInfo = CardInfo
   { getName = ""
   , getCost = 0
   , getValue = 0
   , getTypes = []
   , getTrash = False
   , getPoints = \_ -> return 0
   }

mkCard :: Card a b => a -> VirtualCard b
mkCard card = let fixCard = VirtualCard (cardInfo card) (cardEffect card) (\pid -> validatePlay pid fixCard) in fixCard

vpValue :: Int -> PlayerId -> Dominion Int
vpValue n _ = return n

plusCards :: Int -> PlayerId -> Dominion [VirtualCard ()]
plusCards n pid = do log pid ("+ " ++ show n ++ " cards")
                     cards <- drawFromDeck pid n
                     log pid ("drew " ++ show cards)
                     return cards

plusActions :: Int -> PlayerId -> Dominion Int
plusActions n pid = do log pid ("+ " ++ show n ++ " actions")
                       modifyPlayer pid $ modifyActions (+ n)
                       return n

noop :: PlayerId -> Dominion ()
noop _ = return ()