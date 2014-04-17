module Util where

import Test.Tasty
import Test.Tasty.HUnit

import Data.String
import Control.Monad.State
import Control.Monad.Logger
import Control.Monad.Error

import Dominion
import Dominion.Player
import Dominion.VirtualCard
import qualified Dominion.Cards as CA

instance IsString Player where 
  fromString name = makePlayer name

withHand :: Player -> [VirtualCard ()] -> Player
p `withHand` xs = p{ hand = xs }

withDeck :: Player -> [VirtualCard ()] -> Player
p `withDeck` xs = p{ deck = xs }

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