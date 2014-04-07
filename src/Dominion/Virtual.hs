module Dominion.Virtual where

import Control.Applicative
import Dominion.Types

instance Functor Virtual where
    fmap f (Virtual e s t) = Virtual (\pid -> e pid >>= return . f) s t

instance Applicative Virtual where
    pure x = Virtual (\_ -> return x) (\_ -> return ()) (\_ -> return ())
    -- <*> :: Virtual (a -> b) -> Virtual a -> Virtual b    
    Virtual f1 s1 t1 <*> Virtual f2 s2 t2 = Virtual (\pid -> do f <- f1 pid
                                                                v <- f2 pid
                                                                return (f v))
                                                    (\pid -> s1 pid >> s2 pid)
                                                    (\pid -> t1 pid >> t2 pid)

