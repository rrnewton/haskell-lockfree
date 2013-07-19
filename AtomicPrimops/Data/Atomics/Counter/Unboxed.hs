{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, CPP #-}

module Data.Atomics.Counter.Unboxed
       (AtomicCounter, CTicket,
        newCounter, readCounterForCAS, readCounter, peekCTicket,
        writeCounter, casCounter, incrCounter, incrCounter_)
       where

import GHC.Base
import GHC.Ptr
import Data.Atomics (casByteArrayInt)
import Data.Atomics.Internal (casByteArrayInt#, fetchAddByteArrayInt#)

#ifndef __GLASGOW_HASKELL__
#error "Unboxed Counter: this library is not portable to other Haskell's"
#endif

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

data AtomicCounter = AtomicCounter (MutableByteArray# RealWorld)
type CTicket = Int

{-# INLINE newCounter #-}
newCounter :: Int -> IO AtomicCounter
newCounter n = do
  c <- newRawCounter
  writeCounter c n
  return c

{-# INLINE newRawCounter #-}
newRawCounter :: IO AtomicCounter  
newRawCounter = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, AtomicCounter arr #) }
  where !(I# size) = SIZEOF_HSINT

{-# INLINE readCounter #-}
readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

{-# INLINE writeCounter #-}
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter (AtomicCounter arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

{-# INLINE readCounterForCAS #-}
readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS = readCounter

{-# INLINE peekCTicket #-}
peekCTicket :: CTicket -> Int
peekCTicket !x = x

{-# INLINE casCounter #-}
casCounter :: AtomicCounter -> CTicket -> Int -> IO CTicket
-- casCounter (AtomicCounter barr) !old !new =
casCounter (AtomicCounter mba#) (I# old#) (I# new#) = IO$ \s1# ->
  let (# s2#, res #) = casByteArrayInt# mba# 0# old# new# s1# in
  (# s2#, I# res #)

{-# INLINE sameCTicket #-}
sameCTicket :: CTicket -> CTicket -> Bool
sameCTicket = (==)

{-# INLINE incrCounter #-}
-- | Try repeatedly until we successfully increment the counter by a given amount.
-- Returns the original value of the counter (pre-increment).
--
incrCounter :: Int -> AtomicCounter -> IO Int
incrCounter (I# incr#) (AtomicCounter mba#) = IO $ \ s1# -> 
  let (# s2#, res #) = fetchAddByteArrayInt# mba# 0# incr# s1# in
  (# s2#, (I# res) #)

-- | An alternate version in which we don't care about the old value.
incrCounter_ :: Int -> AtomicCounter -> IO ()
incrCounter_ (I# incr#) (AtomicCounter mba#) = IO $ \ s1# -> 
  let (# s2#, res #) = fetchAddByteArrayInt# mba# 0# incr# s1# in
  (# s2#, () #)
