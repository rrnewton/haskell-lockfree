{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, CPP #-}
-- | Integer counters providing thread-safe, lock-free mutation functions.
--
--   Atomic counters are represented by a single memory location, such that
--   built-in processor instructions are sufficient to perform fetch-and-add or
--   compare-and-swap.
-- 
--   Remember, contention on such counters should still be minimized!

module Data.Atomics.Counter
       -- Reexport to get all the docs.
       (
         -- * Type of counters of counters and tickets
         AtomicCounter, 
         
         -- * Creating counters
         newCounter, 

         -- * Tickets, used for compare-and-swap         
         -- | See the documentation for "Data.Atomics" for more explanation of the
         -- ticket abstraction.  The same ideas apply here for counters as for
         -- general mutable locations (IORefs).
         CTicket, peekCTicket,

         -- * Atomic memory operations
         casCounter, incrCounter, incrCounter_,
                                  
         -- * Non-atomic operations
         readCounter, readCounterForCAS,
         writeCounter
         )
 where


import Data.Atomics.Internal
import GHC.Base  hiding ((==#))
import qualified GHC.PrimopWrappers as GPW


-- GHC 7.8 changed some primops
(==#) :: Int# -> Int# -> Bool
(==#) x y = case x GPW.==# y of { 0# -> False; _ -> True }



#ifndef __GLASGOW_HASKELL__
#error "Counter: this library is not portable to other Haskell's"
#endif

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

-- | The type of mutable atomic counters.
data AtomicCounter = AtomicCounter (MutableByteArray# RealWorld)

-- | You should not depend on this type.  It varies between different implementations
-- of atomic counters.
type CTicket = Int
-- TODO: Could newtype this.

-- | Create a new counter initialized to the given value.
{-# INLINE newCounter #-}
newCounter :: Int -> IO AtomicCounter
newCounter n = do
  c <- newRawCounter
  writeCounter c n -- Non-atomic is ok; it hasn't been released into the wild.
  return c

-- | Create a new, uninitialized counter.
{-# INLINE newRawCounter #-}
newRawCounter :: IO AtomicCounter  
newRawCounter = IO $ \s ->
  case newByteArray# size s of { (# s', arr #) ->
  (# s', AtomicCounter arr #) }
  where !(I# size) = SIZEOF_HSINT

{-# INLINE readCounter #-}
-- | Equivalent to `readCounterForCAS` followed by `peekCTicket`.        
readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s', i #) ->
  (# s', I# i #) }

{-# INLINE writeCounter #-}
-- | Make a non-atomic write to the counter.  No memory-barrier.
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter (AtomicCounter arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s' ->
  (# s', () #) }

{-# INLINE readCounterForCAS #-}
-- | Just like the "Data.Atomics" CAS interface, this routine returns an opaque
-- ticket that can be used in CAS operations.  Except for the difference in return
-- type, the semantics of this are the same as `readCounter`.
readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS = readCounter

{-# INLINE peekCTicket #-}
-- | Opaque tickets cannot be constructed, but they can be destructed into values.
peekCTicket :: CTicket -> Int
peekCTicket !x = x

{-# INLINE casCounter #-}
-- | Compare and swap for the counter ADT.  Similar behavior to
-- `Data.Atomics.casIORef`, in particular, in both success and failure cases it
-- returns a ticket that you should use for the next attempt.  (That is, in the
-- success case, it actually returns the new value that you provided as input, but in
-- ticket form.)
casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
-- casCounter (AtomicCounter barr) !old !new =
casCounter (AtomicCounter mba#) (I# old#) newBox@(I# new#) = IO$ \s1# ->
  let !(# s2#, res# #) = casIntArray# mba# 0# old# new# s1# in
  case res# ==# old# of 
    False -> (# s2#, (False, I# res# ) #) -- Failure
    True  -> (# s2#, (True , newBox ) #) -- Success

-- {-# INLINE sameCTicket #-}
-- sameCTicket :: CTicket -> CTicket -> Bool
-- sameCTicket = (==)

{-# INLINE incrCounter #-}
-- | Increment the counter by a given amount.  Returns the value AFTER the increment
--   (in contrast with the behavior of the underlying instruction on architectures
--   like x86.)
--                 
--   Note that UNLIKE with boxed implementations of counters, where increment is
--   based on CAS, this increment is /O(1)/.  Fetch-and-add does not require a retry
--   loop like CAS.
incrCounter :: Int -> AtomicCounter -> IO Int
incrCounter (I# incr#) (AtomicCounter mba#) = IO $ \ s1# -> 
  let !(# s2#, res #) = fetchAddIntArray# mba# 0# incr# s1# in
  (# s2#, (I# (res +# incr#)) #)

{-# INLINE incrCounter_ #-}
-- | An alternate version for when you don't care about the old value.
incrCounter_ :: Int -> AtomicCounter -> IO ()
incrCounter_ (I# incr#) (AtomicCounter mba#) = IO $ \ s1# -> 
  let !(# s2#, _ #) = fetchAddIntArray# mba# 0# incr# s1# in
  (# s2#, () #)
