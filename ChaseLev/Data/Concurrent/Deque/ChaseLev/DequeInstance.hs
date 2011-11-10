{-# LANGUAGE TypeFamilies #-}

module Data.Concurrent.Deque.ChaseLev.DequeInstance () where

import Data.Array.IO
import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.ChaseLev
import qualified Data.Concurrent.Deque.ReactorDeque as R

-- Work stealing queues are only threadsafe on one end and
-- double-functionality on the other:
type instance Deque NT T D S Grow Safe elt = R.Deque IOArray elt

-- [2011.11.09] Presently having problems with this error when I try
-- to use these Deques:
--
--    Couldn't match type `Deque
--                            Nonthreadsafe Threadsafe DoubleEnd SingleEnd Grow Safe (Par ())'
--                    with `R.Deque IOArray (Par ())'
