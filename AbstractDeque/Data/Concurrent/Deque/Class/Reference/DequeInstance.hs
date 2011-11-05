{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Data.Concurrent.Deque.Class.Reference.DequeInstance () where

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.Class.Reference as R

--  The reference implementation is a fully general Deque.  It can
--   thus cover the full configuration space.
-- type instance Deque lt rt l r bnd safe elt = R.SimpleDeque elt

--  The reference implementation is a general, growable Deque.  It can
--  cover half of the configuration space.  It does not provide a
--  bounded queue.
type instance Deque lt rt l r bnd safe elt = R.SimpleDeque elt
