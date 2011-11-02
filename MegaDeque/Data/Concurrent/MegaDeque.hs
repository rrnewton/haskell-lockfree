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
import Data.Concurrent.LinkedQueue

-- Fallback implementation:
-- import Data.Concurrent.Deque.Class.Reference 


-- PRETEND IMPLEMENTATION:
-- Non-concurrent circular buffers:
-- import Data.Deque.CircBuffer
newtype CircBuf elt = CircBuf elt -- TEMP
instance DequeClass CircBuf where

------------------------------------------------------------
-- Single-ended Queues:

newtype instance Deque lt rt S S bnd safe elt = LQQ (LinkedQueue elt)
--data instance Deque T T S S Grow Safe elt = LinkedQueue elt

instance DequeClass (Deque lt rt S S bnd safe) where 
  newQ            = newQ >>= (return . LQQ)
  pushL   (LQQ q) = pushL q
  tryPopR (LQQ q) = tryPopR q


------------------------------------------------------------
-- All others:

-- It is possible, though extremely verbose, to enumerate whichever of the
-- 64 possibilities remain here.

-- HOWEVER, for the free type-variable 'elt' it is not possible to
-- specialize at some types and still have a fallback.  This is
-- because no overlap is permitted for type families. 

newtype instance Deque lt rt D S bnd safe elt = CQQ1 (CircBuf elt)
newtype instance Deque lt rt D D bnd safe elt = CQQ2 (CircBuf elt)
newtype instance Deque lt rt S D bnd safe elt = CQQ3 (CircBuf elt)

instance DequeClass (Deque lt rt D S bnd safe) where
  newQ             = newQ >>= (return . CQQ1)
  pushL   (CQQ1 q) = pushL q
  tryPopR (CQQ1 q) = tryPopR q

instance DequeClass (Deque lt rt D D bnd safe) where 
  newQ             = newQ >>= (return . CQQ2)
  pushL   (CQQ2 q) = pushL q
  tryPopR (CQQ2 q) = tryPopR q

instance DequeClass (Deque lt rt S D bnd safe) where 
  newQ             = newQ >>= (return . CQQ3)
  pushL   (CQQ3 q) = pushL q
  tryPopR (CQQ3 q) = tryPopR q

