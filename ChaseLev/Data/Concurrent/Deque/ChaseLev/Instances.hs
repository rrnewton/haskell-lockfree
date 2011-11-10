{-# LANGUAGE TypeFamilies #-}

module Data.Concurrent.Deque.ChaseLev.Instances () where

import Data.Array.IO

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.ChaseLev
import qualified Data.Concurrent.Deque.ReactorDeque as R

type instance Deque NT T D S Grow Safe elt = R.Deque IOArray elt

--    Couldn't match type `Deque
--                            Nonthreadsafe Threadsafe DoubleEnd SingleEnd Grow Safe (Par ())'
--                    with `R.Deque IOArray (Par ())'