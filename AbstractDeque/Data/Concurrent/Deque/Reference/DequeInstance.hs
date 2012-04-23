{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

{- | 

  By convention, every provider of the "Data.Concurrent.Deque.Class"
  interface optionally provides a module that provides the relevant
  instances of the 'Deque' type class, covering the [maximum] portion
  of the configuration space that the implementation is able to
  handle.

  This is kept in a separate package because importing instances is
  unconditional and the user may well want to assemble their own
  combination of 'Deque' instances to cover the configuration
  space.
 -}

module Data.Concurrent.Deque.Reference.DequeInstance () where

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.Reference as R

-- | The reference implementation is a fully general Deque.  It can
--   thus cover the full configuration space.
type instance Deque lt rt l r bnd safe elt = R.SimpleDeque elt

