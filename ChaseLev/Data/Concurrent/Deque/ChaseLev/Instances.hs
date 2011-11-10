{-# LANGUAGE TypeFamilies #-}

module Data.Concurrent.Deque.Class.Reference.DequeInstance () where

import Data.Array.IO

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.ChaseLev
import qualified Data.Concurrent.Deque.ReactorDeque as R

type instance Deque T NT D S Grow Safe elt = R.Deque IOArray elt

