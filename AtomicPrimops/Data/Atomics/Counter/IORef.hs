{-# LANGUAGE BangPatterns #-}

-- | This version uses a boxed IORef representation, but it can be
-- somewhat cheaper because it uses raw CAS rather than full atomicModifyIORef.

module Data.Atomics.Counter.IORef
       where

import Data.IORef
import Data.Atomics as A

--------------------------------------------------------------------------------

-- type AtomicCounter = IORef Int
newtype AtomicCounter = AtomicCounter (IORef Int)

type CTicket = Ticket Int

-- | Create a new counter initialized to zero.
newCounter :: IO AtomicCounter
newCounter = fmap AtomicCounter $ newIORef 0

-- | Try repeatedly until we successfully increment the counter.
-- incrCounter =

readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS (AtomicCounter r) = readForCAS r

peekCTicket :: CTicket -> Int
peekCTicket = peekTicket 

readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter r) = readIORef r

-- | Make a non-atomic write to the counter.  No memory-barrier.
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter (AtomicCounter r) !new = writeIORef r new

casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
casCounter (AtomicCounter r) tick !new = casIORef r tick new
