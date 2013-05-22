

-- | This module provides a wrapper around a deque that can enforce additional
-- invariants at runtime for debugging purposes.

module Data.Concurrent.Deque.Debugger
       ()
       where

import Data.IORef
import Control.Concurrent
import Data.Concurrent.Deque.Class

-- newtype DebugDeque d = DebugDeque d

-- | Warning, this enforces the excessively STRONG invariant that if any end of the
-- deque is non-threadsafe then it may every only be touched by one thread during its
-- entire lifetime.
--
-- This extreme form of monagamy is easier to verify, because we don't have enough
-- information to know if two operations on different threads are racing with one
-- another or are properly synchronized.
data DebugDeque d elt = DebugDeque (IORef (Maybe ThreadId)) (d elt) 


instance DequeClass d => DequeClass (DebugDeque d) where 
  pushL (DebugDeque d ref) elt =
    undefined

  newQ = do r <- newIORef Nothing
            fmap (DebugDeque r) newQ

