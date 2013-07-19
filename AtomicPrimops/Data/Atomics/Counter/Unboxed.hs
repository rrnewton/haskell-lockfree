{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, CPP #-}

module Data.Atomics.Counter.Unboxed
       where

import GHC.Base
import GHC.Ptr
import Data.Atomics (casByteArrayInt)
import Data.Atomics.Internal (casByteArrayInt#)

#ifndef __GLASGOW_HASKELL__
#error "Unboxed Counter: this library is not portable to other Haskell's"
#endif

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

data AtomicCounter = AtomicCounter (MutableByteArray# RealWorld)
type CTicket = Int

newCounter :: IO AtomicCounter
readCounter :: AtomicCounter -> IO Int
writeCounter :: AtomicCounter -> Int -> IO ()

newCounter = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, AtomicCounter arr #) }
  where !(I# size) = SIZEOF_HSINT

readCounter (AtomicCounter arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeCounter (AtomicCounter arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }


readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS = readCounter

peekCTicket :: CTicket -> Int
peekCTicket !x = x

casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
-- casCounter (AtomicCounter barr) !old !new =
casCounter (AtomicCounter mba#) (I# old#) (I# new#) = IO$ \s1# ->
  -- case casByteArrayInt# mba# ix# old# new# s1# of
  --   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, I# res) #)

  let (# s2#, x#, res #) = casByteArrayInt# mba# 0# old# new# s1# in
  (# s2#, (x# ==# 0#, I# res) #)
