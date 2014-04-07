module Util where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.State
import Control.Monad.Logger
import Control.Monad.Error

import Dominion
import Dominion.Player
import Dominion.VirtualCard
import qualified Dominion.Cards as CA

withHand :: String -> [VirtualCard ()] -> Player
playerName `withHand` xs = Player
                         { playerName = playerName
                         , discard =  (7 `cardsOf` CA.copper ++ 3 `cardsOf` CA.estate)
                         , deck = []
                         , hand = xs
                         , actions = 1
                         , buys = 1
                         , money = 0
                         }

withPlayers :: [Player] -> Dominion a -> Assertion
withPlayers players f = do gs <- makeGameState defaultOptions players
                           result <- runLoggingT (evalStateT (runErrorT $ runDominion f) gs) (\_ _ _ _ -> return ())
                           case result of 
                             Left x -> assertFailure x
                             Right _ -> return ()

expectErrorWithPlayers :: [Player] -> Dominion a -> Assertion
expectErrorWithPlayers players f = do gs <- makeGameState defaultOptions players
                                      result <- runLoggingT (evalStateT (runErrorT $ runDominion f) gs) (\_ _ _ _ -> return ()) 
                                      case result of 
                                        Left _ -> return ()
                                        Right _ -> assertFailure "expected an exception but found a value"