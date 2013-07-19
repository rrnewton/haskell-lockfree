{-# LANGUAGE BangPatterns #-}

-- | This implementation stores an unboxed counter and uses FFI operations to modify
-- its contents.  It has the advantage that it can use true fetch-and-add operations.
-- It has the disadvantage of extra overhead due to FFI calls.

module Data.Atomics.Counter.Foreign
       (AtomicCounter, CTicket,
        newCounter, readCounterForCAS, readCounter, peekCTicket,
        writeCounter, casCounter, incrCounter, incrCounter_)
   where
import Control.Monad (void)
import Data.Bits.Atomic
import Foreign.ForeignPtr
import Foreign.Storable

-- newtype AtomicCounter = AtomicCounter (ForeignPtr Int)
type AtomicCounter = ForeignPtr Int

type CTicket = Int

{-# INLINE newCounter #-}
-- | Create a new counter initialized to the given value.
newCounter :: Int -> IO AtomicCounter
newCounter n = do x <- mallocForeignPtr
                  writeCounter x n
                  -- Do we need a write barrier here?
                  return x

{-# INLINE incrCounter #-}
-- | Increment the counter by a given amount.
--   Returns the original value before the increment.
--                 
--   Note that UNLIKE with boxed implementations of counters, where increment is
--   based on CAS, this increment is /O(1)/.  Fetch-and-add does not require a retry
--   loop like CAS.
incrCounter :: Int -> AtomicCounter -> IO Int
incrCounter bump r = withForeignPtr r$ \r' -> fetchAndAdd r' bump

{-# INLINE incrCounter_ #-}
-- | An alternate version for when you don't care about the old value.
incrCounter_ :: Int -> AtomicCounter -> IO ()
incrCounter_ bump r = withForeignPtr r$ \r' -> void (fetchAndAdd r' bump)

{-# INLINE readCounterForCAS #-}
-- | Just like the "Data.Atomics" CAS interface, this routine returns an opaque
-- ticket that can be used in CAS operations.
readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS = readCounter

{-# INLINE peekCTicket #-}
-- | Opaque tickets cannot be constructed, but they can be destructed into values.
peekCTicket :: CTicket -> Int
peekCTicket x = x

{-# INLINE readCounter #-}
-- | Equivalent to `readCounterForCAS` followed by `peekCTicket`.
readCounter :: AtomicCounter -> IO Int
readCounter r = withForeignPtr r peek 

{-# INLINE writeCounter #-}
-- | Make a non-atomic write to the counter.  No memory-barrier.
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter r !new = withForeignPtr r $ \r' -> poke r' new

{-# INLINE casCounter #-}
-- | Compare and swap for the counter ADT.
casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
casCounter r !tick !new = withForeignPtr r $ \r' -> do
   b <- compareAndSwap r' tick new
   -- if b then return (True,new)
   --      else do x <- peek r'
   --              return (False,x)
   return (b==tick, b)
