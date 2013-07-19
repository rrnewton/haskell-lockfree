{-# LANGUAGE BangPatterns #-}

-- | This version uses a boxed IORef representation, but it can be somewhat cheaper
-- than the Refence version because it uses raw CAS rather than full
-- atomicModifyIORef.

module Data.Atomics.Counter.IORef
       (AtomicCounter, CTicket,
        newCounter, readCounterForCAS, readCounter, peekCTicket,
        writeCounter, casCounter, incrCounter, incrCounter_)
       where

import Control.Monad (void)
import Data.IORef
import Data.Atomics as A

--------------------------------------------------------------------------------

-- type AtomicCounter = IORef Int
newtype AtomicCounter = AtomicCounter (IORef Int)

type CTicket = Ticket Int

{-# INLINE newCounter #-}
-- | Create a new counter initialized to the given value.
newCounter :: Int -> IO AtomicCounter
newCounter n = fmap AtomicCounter $ newIORef n

{-# INLINE incrCounter #-}
-- | Try repeatedly until we successfully increment the counter by a given amount.
-- Returns the original value of the counter (pre-increment).
incrCounter :: Int -> AtomicCounter -> IO Int
-- <DUPLICATED CODE FROM Reference.hs>
incrCounter bump cntr =
    loop =<< readCounterForCAS cntr
  where
    loop tick = do
      (b,tick') <- casCounter cntr tick (peekCTicket tick + bump)
      if b then return (peekCTicket tick')
           else loop tick'
{-# INLINE incrCounter_ #-}
incrCounter_ :: Int -> AtomicCounter -> IO ()
incrCounter_ b c = void (incrCounter b c)
-- </DUPLICATED CODE FROM Reference.hs>

{-# INLINE readCounterForCAS #-}
-- | Just like the "Data.Atomics" CAS interface, this routine returns an opaque
-- ticket that can be used in CAS operations.
readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS (AtomicCounter r) = readForCAS r

{-# INLINE peekCTicket #-}
-- | Opaque tickets cannot be constructed, but they can be destructed into values.
peekCTicket :: CTicket -> Int
peekCTicket = peekTicket 

{-# INLINE readCounter #-}
-- | Equivalent to `readCounterForCAS` followed by `peekCTicket`.
readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter r) = readIORef r

{-# INLINE writeCounter #-}
-- | Make a non-atomic write to the counter.  No memory-barrier.
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter (AtomicCounter r) !new = writeIORef r new

{-# INLINE casCounter #-}
-- | Compare and swap for the counter ADT.
casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
casCounter (AtomicCounter r) tick !new = casIORef r tick new
