module DominionTest.Cards.Original where

import Test.Tasty
import Test.Tasty.HUnit
import Util

import Dominion
import Dominion.Internal
import Dominion.Cards.Original
import Dominion.Card
import Dominion.Player

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

originalCardTests = testGroup "Original Card Tests" 
    [ testCase "playing a village yields two actions and one card" $
        withPlayers ["Ben" `withHand` [void village]] $ do
            p <- getPlayer 0
            let a = actions p
                h = hand p
            result <- 0 `playsCard_` village
            liftIO $ assertEqual "should draw one card" 1 (length result)
            p' <- getPlayer 0 
            liftIO $ assertEqual "should have one more action" (a + 1) (actions p')
            liftIO $ assertEqual "should have the same number of cards" (length h) (length $ hand p')

    , testCase "playing a smithy yields three cards" $
        withPlayers ["Ben" `withHand` [void smithy]] $ do
            p <- getPlayer 0
            let h = hand p
            result <- 0 `playsCard_` smithy
            liftIO $ assertEqual "should draw three cards" 3 (length result)
            p' <- getPlayer 0 
            liftIO $ assertEqual "should have two more cards" (length h + 2) (length $ hand p')

    , testCase "playing a throne room with an action card yields cards action twice" $
        withPlayers ["Ben" `withHand` [void throneRoom, void smithy]] $ do
            p <- getPlayer 0
            let h = hand p
            result <- 0 `plays` (ThroneRoom `with` smithy)
            liftIO $ assertEqual "should draw three cards, twice" (3, 3) ((length *** length) result)
            p' <- getPlayer 0 
            liftIO $ assertEqual "should have 4 more cards" (length h + 4) (length $ hand p')

    ]