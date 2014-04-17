{-# LANGUAGE OverloadedStrings #-}

module DominionTest.Cards.Base where

import Test.Tasty
import Test.Tasty.HUnit

import Dominion
import Dominion.Card
import Dominion.Cards.Original
import Dominion.Cards.Base
import Dominion.Internal
import Dominion.Player
import Util

import Control.Monad
import Control.Monad.Trans

baseCardTests = testGroup "Base Card Tests"
  [ testCase "copper provides 1 money" $
      withPlayers ["Ben" `withHand` [copper]] $ do
        v <- handValue 0
        liftIO $ v @=? 1
  
  , testCase "silver provides 2 money" $
      withPlayers ["Ben" `withHand` [silver]] $ do
        v <- handValue 0
        liftIO $ v @=? 2
        
  , testCase "gold provides 3 money" $
      withPlayers ["Ben" `withHand` [gold]] $ do
        v <- handValue 0
        liftIO $ v @=? 3
  ]