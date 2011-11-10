{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Data.Concurrent.Deque.Reference.DequeInstance () where

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.Reference as R

--  The reference implementation is a fully general Deque.  It can
--   thus cover the full configuration space.
type instance Deque lt rt l r bnd safe elt = R.SimpleDeque elt

