module Dominion.VirtualCard (VirtualCard (..)) where

import Control.Applicative
import Data.Ord
import Dominion.Types

instance Functor VirtualCard where
    fmap f (VirtualCard i e s) = VirtualCard i (\pid -> e pid >>= return . f) s

instance Eq (VirtualCard a) where
    (VirtualCard p1 _ _) == (VirtualCard p2 _ _) = (getName p1) == (getName p2)

instance Ord (VirtualCard a) where
    compare = comparing (getName . info)

instance Show (VirtualCard a) where
    show (VirtualCard c _ _) = getName c