module Dominion.Player 
    (
      Player (..)
    , modifyDeck
    , modifyHand
    , modifyDiscard
    , modifyActions
    , modifyBuys
    , modifyMoney
    , resetPlayer
    )

where

import Dominion.Types

{-

data Player where
    Player :: String -- name
           -> CardList -- deck
           -> CardList -- hand
           -> CardList -- discard
           -> Int -- actions
           -> Int -- buys
           -> Int -- money
           -> Player

-}

modifyDeck :: (CardList -> CardList) -> Player ->  Player
modifyDeck f player = player{deck=(f (deck player))}

modifyHand :: (CardList -> CardList) -> Player ->  Player
modifyHand f player = player{hand=(f (hand player))}

modifyDiscard :: (CardList -> CardList) -> Player -> Player
modifyDiscard f player = player{discard=(f (discard player))}

modifyActions :: (Int -> Int) -> Player ->  Player
modifyActions f player = player{actions=(f (actions player))}

modifyBuys :: (Int -> Int) -> Player ->  Player
modifyBuys f player = player{buys=(f (buys player))}

modifyMoney :: (Int -> Int) -> Player -> Player
modifyMoney f player = player{money=(f (money player))}

resetPlayer :: Player -> Player
resetPlayer player = player{actions=1, buys=1, money=0}

