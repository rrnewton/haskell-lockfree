{-# LANGUAGE TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverlappingInstances
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

-- Non-concurrent circular buffers:
-- import Data.Deque.CircBuffer
newtype CircBuf elt = CircBuf elt -- TEMP

------------------------------------------------------------
-- Single-ended Queues:

-- Linked Queue should already export instances for SingleEnd/SingleEnd:

-- newtype instance Deque lt rt S S bnd safe elt = LQQ (LinkedQueue elt)
-- --data instance Deque T T S S Grow Safe elt = LinkedQueue elt

-- instance DequeClass (Deque lt rt S S bnd safe) where 
--   newQ            = newQ >>= (return . LQQ)
--   pushL   (LQQ q) = pushL q
--   tryPopR (LQQ q) = tryPopR q


-- instance DequeClass lt rt SingleEnd SingleEnd bnd safe elt where 
--   newtype Deque lt rt SingleEnd SingleEnd bnd safe elt = LinkedQueue (CircBuf elt)
--   newQ             = undefined
--   pushL   (CQQ1 q) = undefined
--   tryPopR (CQQ1 q) = undefined


------------------------------------------------------------
-- All others:

-- -- It is possible, though extremely verbose, to enumerate whichever of the
-- -- 64 possibilities remain here.

-- -- HOWEVER, for the free type-variable 'elt' it is not possible to
-- -- specialize at some types and still have a fallback.  This is
-- -- because no overlap is permitted for type families. 

-- instance DequeClass lt rt DoubleEnd SingleEnd bnd safe elt where 
--   newtype Deque lt rt DoubleEnd SingleEnd bnd safe elt = CQQ1 (CircBuf elt)
--   newQ             = undefined
--   pushL   (CQQ1 q) = undefined
--   tryPopR (CQQ1 q) = undefined

-- -- This kind of thing won't work due to overlapping instances:
-- instance DequeClass lt rt DoubleEnd SingleEnd bnd safe Int where 
--   newtype Deque lt rt DoubleEnd SingleEnd bnd safe Int = CQQ2 (CircBuf Int)
--   newQ             = undefined
--   pushL   (CQQ2 q) = undefined
--   tryPopR (CQQ2 q) = undefined

class NonForeignQueable t where 
class ForeignQueable t where 

instance NonForeignQueable t => DequeClass lt rt DoubleEnd SingleEnd bnd safe t where 
  newtype Deque lt rt DoubleEnd SingleEnd bnd safe t = CQQ1 (CircBuf t)
  newQ             = undefined
  pushL   (CQQ1 q) = undefined
  tryPopR (CQQ1 q) = undefined

instance ForeignQueable t => DequeClass lt rt DoubleEnd SingleEnd bnd safe t where 
  newtype Deque lt rt DoubleEnd SingleEnd bnd safe t = CQQ2 (CircBuf t)
  newQ             = undefined
  pushL   (CQQ2 q) = undefined
  tryPopR (CQQ2 q) = undefined
