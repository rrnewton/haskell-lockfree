{-# LANGUAGE TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

-- | Multiplex multiple queue implementations together using a type family.
module Data.Concurrent.MegaDeque

  where


-- Abstract interface:
import Data.Concurrent.Deque.Class

-- Michael and Scott Queues:
import Data.Concurrent.Queue.MichaelScott

-- Fallback implementation:
import Data.Concurrent.Deque.Reference

------------------------------------------------------------
-- Single-ended Queues:

-- | This instance classifies the LinkedQueue as being single-ended and fully threadsafe.

-- newtype instance Deque lt rt S S bnd safe elt = LQQ (LinkedQueue elt)
type instance Deque lt rt S S bnd safe elt = LinkedQueue elt


------------------------------------------------------------
-- All others:

-- It is necessary to enumerate whichever of the 64 possibilities
-- remain here.

-- But for the free type-variable 'elt' it is not possible to
-- specialize at some types and still have a fallback.  This is
-- because no overlap is permitted for type families.

type instance Deque lt rt D S bnd safe elt = SimpleDeque elt
type instance Deque lt rt D D bnd safe elt = SimpleDeque elt
type instance Deque lt rt S D bnd safe elt = SimpleDeque elt
