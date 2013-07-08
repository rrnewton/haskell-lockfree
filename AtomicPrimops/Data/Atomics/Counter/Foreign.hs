{-# LANGUAGE BangPatterns #-}

-- | This implementation stores an unboxed counter and uses FFI
-- operations to modify its contents.

module Data.Atomics.Counter.Foreign
   where

import Data.Bits.Atomic
import Foreign.ForeignPtr
import Foreign.Storable

-- newtype AtomicCounter = AtomicCounter (ForeignPtr Int)
type AtomicCounter = ForeignPtr Int

type CTicket = Int

-- | Create a new counter initialized to zero.
newCounter :: IO AtomicCounter
newCounter = do x <- mallocForeignPtr
                writeCounter x 0
                return x

-- | Try repeatedly until we successfully increment the counter.
-- Returns the original value before the increment.
incrCounter :: AtomicCounter -> IO Int
incrCounter r = withForeignPtr r$ \r' -> fetchAndAdd r' 1 

readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS = readCounter

peekCTicket :: CTicket -> Int
peekCTicket x = x

readCounter :: AtomicCounter -> IO Int
readCounter r = withForeignPtr r peek 

-- | Make a non-atomic write to the counter.  No memory-barrier.
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter r !new = withForeignPtr r $ \r' -> poke r' new

casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
casCounter r !tick !new = withForeignPtr r $ \r' -> do
   b <- compareAndSwap r' tick new
   -- if b then return (True,new)
   --      else do x <- peek r'
   --              return (False,x)
   return (b==tick, b)
