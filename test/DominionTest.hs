{-# LANGUAGE OverloadedStrings #-}

module DominionTest where

import Test.Tasty
import Test.Tasty.HUnit

import Dominion
import Dominion.Card
import Dominion.Cards.Original
import Dominion.Internal
import Dominion.Player
import Util

import Control.Monad
import Control.Monad.Trans

dominionTests = testGroup "Dominion Tests"
  [ testCase "playsCard with card in hand" $
      withPlayers ["Ben" `withHand` [void village]] $ do
        0 `playsCard_` village
        player <- getPlayer 0
        let h = hand player
        liftIO $ forM_ h $ \c -> name c /= name village @? "Played card still in hand"
  
  , testCase "playsCard without card in hand" $
      expectErrorWithPlayers ["Ben" `withHand` []] $ do
        0 `playsCard_` village
        return ()
  ]