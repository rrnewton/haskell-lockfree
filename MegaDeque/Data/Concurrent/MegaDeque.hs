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
import qualified Data.Concurrent.Queue.MichaelScott as MS
import qualified Data.Concurrent.Deque.ChaseLev  as CL

-- Fallback implementation:
import qualified Data.Concurrent.Deque.Reference as R

------------------------------------------------------------
-- Single-ended Queues:

-- | This instance classifies the LinkedQueue as being single-ended and fully threadsafe.

-- newtype instance Deque lt rt S S bnd safe elt = LQQ (LinkedQueue elt)
type instance Deque lt rt S S bnd safe elt = MS.LinkedQueue elt

-- Work stealing queues are only threadsafe on one end (pop-only) and
-- double (push/pop) functionality on the other:
type instance Deque NT rt D S bnd safe elt = CL.ChaseLevDeque elt

------------------------------------------------------------
-- All others:

-- It is necessary to enumerate whichever of the 64 possibilities
-- remain here.

-- But for the free type-variable 'elt' it is not possible to
-- specialize at some types and still have a fallback.  This is
-- because no overlap is permitted for type families in GHC <= 7.6.

type instance Deque lt rt D D bnd safe elt = R.SimpleDeque elt
type instance Deque lt rt S D bnd safe elt = R.SimpleDeque elt
-- Catch what isn't handled by chaselev:
type instance Deque T  rt D S bnd safe elt = R.SimpleDeque elt

