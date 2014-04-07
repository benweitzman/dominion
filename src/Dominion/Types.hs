{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dominion.Types where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Logger
import qualified Data.Map as M

data CardType = Action
              | Attack
              | Reaction
              | Treasure
              | Victory
              | Duration
              deriving (Show, Eq, Ord)

class HasCardInfo a where
    name :: a -> String
    cost :: a -> Int
    value :: a -> Int
    types :: a -> [CardType]
    trash :: a -> Bool
    points :: a -> PlayerId -> Dominion Int

data CardInfo = CardInfo
      { getName   :: String
      , getCost   :: Int
      , getValue  :: Int
      , getTypes  :: [CardType]
      , getTrash  :: Bool
      , getPoints :: PlayerId -> Dominion Int
      }

instance HasCardInfo CardInfo where
  name = getName
  cost = getCost
  value = getValue
  types = getTypes
  trash = getTrash
  points = getPoints

data Virtual a = Virtual (PlayerId -> Dominion a) (PlayerId -> Dominion ()) (PlayerId -> Dominion ())
data VirtualCard a = VirtualCard 
  { info :: CardInfo
  , effect :: PlayerId -> Dominion a
  , setup :: PlayerId -> Dominion ()
  }

class Virtualizable a b | a -> b where
  toVirtual :: a -> Virtual b

instance HasCardInfo (VirtualCard a) where
  name = getName . info
  cost = getCost . info
  value = getValue . info
  types = getTypes . info
  trash = getTrash . info
  points = getPoints . info

class Card a b | a -> b where
    cardInfo :: a -> CardInfo
    cardEffect :: a -> PlayerId -> Dominion b

{- 
instance Card a b => HasCardInfo a where
  name = getName . cardInfo
  cost = getCost . cardInfo
  value = getValue . cardInfo
  types = getTypes . cardInfo
  trash = getTrash . cardInfo
  points = getPoints . cardInfo
-}

class Effectful a b c | a b -> c where
    with :: Virtualizable b d => a -> b -> Virtual c

data GameStats = GameStats {
  -- | playerId -> victory points
  victoryPoints :: M.Map PlayerId Int
}

data GameState = GameState 
    { cards    :: M.Map (VirtualCard ()) Int -- cards
    , players  :: [Player] -- players
    , roundNum :: Int -- round
    , verbose  :: Bool
    , stats    :: [GameStats]
    }

data Player = Player
    { playerName :: String -- name
    , hand       :: [VirtualCard ()] -- hand
    , deck       :: [VirtualCard ()] -- deck
    , discard    :: [VirtualCard ()] -- discard
    , actions    :: Int -- actions
    , buys       :: Int -- buys
    , money      :: Int -- money
    }

newtype Dominion a = Dominion { runDominion :: ErrorT String (StateT GameState (LoggingT IO)) a } deriving
  (Monad, MonadState GameState, MonadError String, MonadIO, MonadLogger, Functor)

type PlayerId = Int

type Strategy = PlayerId -> Dominion ()

type CardList = [VirtualCard ()]

-- | Each `PlayerResult` is a tuple of a player and their final score.
type PlayerResult = (Player, Int)

-- | Players and their scores.
data Result = Result
    { playerResults :: [PlayerResult]
    , winner        :: String
    }

