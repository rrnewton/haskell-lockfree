{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Data.Concurrent.Queue.MichaelScott.DequeInstance () where

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Queue.MichaelScott as M

-- | This queue is not fully general, it covers only part of the
--   configuration space:
type instance Deque lt rt S S bnd safe elt = M.LinkedQueue elt
