-- | Provides atomic memory operations on IORefs and Mutable Arrays.
--
--   Pointer equality need not be maintained by a Haskell compiler.  For example, Int
--   values will frequently be boxed and unboxed, changing the pointer identity of
--   the thunk.  To deal with this, the compare-and-swap (CAS) approach used in this
--   module is uses a /sealed/ representation of pointers into the Haskell heap
--   (`Tickets`).  Currently, the user cannot coin new tickets, rather a `Ticket`
--   provides evidence of a past observation, and grants permission to make a future
--   change.
module Data.Atomics
 (
   -- * Types for atomic operations
   Ticket, peekTicket, -- CASResult(..),

   -- * Atomic operations on references
   readForCAS, casRef, casRef2,
   atomicModifyRefCAS, atomicModifyRefCAS_,

   -- * Atomic operations on mutable arrays
   casArrayElem, casArrayElem2, readArrayElem,

   -- * Atomic operations on byte arrays
   casByteArrayInt,
   fetchAddIntArray,
   fetchSubIntArray,
   fetchAndIntArray,
   fetchNandIntArray,
   fetchOrIntArray,
   fetchXorIntArray,
   -- -- ** Reading and writing with barriers
   -- atomicReadIntArray,
   -- atomicWriteIntArray,

   -- * Atomic operations on raw MutVars
   -- | A lower-level version of the IORef interface.
   readMutVarForCAS, casMutVar, casMutVar2,

   -- * Memory barriers
   storeLoadBarrier, loadLoadBarrier, writeBarrier,

   -- * Deprecated Functions
   fetchAddByteArrayInt,
   casIORef, casIORef2,
   atomicModifyIORefCAS, atomicModifyIORefCAS_,
 ) where

import Data.Atomics.Class
import Data.Atomics.Internal
import Data.IORef

{- WIP. Having trouble writing good tests for these, and not sure how useful
 - these are. See #43 discussion
 -
 - Also remember to add these to the INLINE / NOINLINE section when exported

-- imports for GHC < 7.10 conditionals below.
#if MIN_VERSION_base(4,8,0)
#else
import Control.Monad (void)
import Data.Primitive.ByteArray (writeByteArray)
#endif


-- | Given an array and an offset in Int units, read an element. The index is
-- assumed to be in bounds. Implies a full memory barrier.
atomicReadIntArray :: MutableByteArray RealWorld -> Int -> IO Int
#if MIN_VERSION_base(4,8,0)
atomicReadIntArray (MutableByteArray mba#) (I# ix#) = IO $ \ s# ->
    case atomicReadIntArray# mba# ix# s# of
        (# s2#, n# #) -> (# s2#, I# n# #)
#else
atomicReadIntArray mba ix = do
    -- I don't think we can get a full barrier here with the three barriers we
    -- have exposed, so we use a no-op CAS, which implies a full barrier
    casByteArrayInt mba ix 0 0
{-# WARNING atomicReadIntArray "atomicReadIntArray is implemented with a CAS on GHC <7.10 and may be slower than a readByteArray + one of the barriers exposed here" #-}
#endif

-- | Given an array and an offset in Int units, write an element. The index is
-- assumed to be in bounds. Implies a full memory barrier.
atomicWriteIntArray :: MutableByteArray RealWorld -> Int -> Int -> IO ()
#if MIN_VERSION_base(4,8,0)
atomicWriteIntArray (MutableByteArray mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case atomicWriteIntArray# mba# ix# n# s# of
        s2# -> (# s2#, () #)
#else
atomicWriteIntArray mba ix n = do
    -- As above we use a no-op CAS to get a full barrier. This is particularly
    -- gross TODO something better if possible
    let fullBarrier = void $ casByteArrayInt mba ix 0 0
    fullBarrier
    writeByteArray mba ix n
    fullBarrier
{-# WARNING atomicWriteIntArray "atomicWriteIntArray is likely to be very slow on GHC <7.10. Consider using writeByteArray along with one of the barriers exposed here instead" #-}
#endif

-}

-- * Old functions

{-# DEPRECATED casIORef              "Replaced by casRef which has a more generic type" #-}
{-# DEPRECATED casIORef2             "Replaced by casRef2 which has a more generic type" #-}
{-# DEPRECATED atomicModifyIORefCAS  "Replaced by atomicModifyRefCAS which has a more generic type" #-}
{-# DEPRECATED atomicModifyIORefCAS_ "Replaced by atomicModifyRefCAS_ which has a more generic type" #-}
{-# INLINE casIORef #-}
{-# INLINE casIORef2 #-}
{-# INLINE atomicModifyIORefCAS #-}
{-# INLINE atomicModifyIORefCAS_ #-}

-- | Performs a machine-level compare and swap (CAS) operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the most 'current' value from the
-- 'IORef'.  Note that this differs from the more common CAS behavior,
-- which is to return the /old/ value before the CAS occured.
--
-- The reason for the difference is the ticket API.  This function
-- always returns the ticket that you should use in your next CAS
-- attempt.  In case of success, this ticket corresponds to the `new`
-- value which you yourself installed in the `IORef`, whereas in the
-- case of failure it represents the preexisting value currently in
-- the IORef.
--
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.  However, the ticket API
-- absolves the user of this module from needing to worry about the
-- pointer equality of their values, which in general requires
-- reasoning about the details of the Haskell implementation (GHC).
--
-- By convention this function is strict in the "new" value argument.
-- This isn't absolutely necesary, but we think it's a bad habit to
-- use unevaluated thunks in this context.
casIORef :: IORef a -> Ticket IO a -> a -> IO (Bool, Ticket IO a)
casIORef = casRef

-- | This variant takes two tickets, i.e. the 'new' value is a ticket
-- rather than an arbitrary, lifted, Haskell value.
casIORef2 :: IORef a -> Ticket IO a -> Ticket IO a -> IO (Bool, Ticket IO a)
casIORef2 = casRef2

-- | A drop-in replacement for `atomicModifyIORefCAS` that
-- optimistically attempts to compute the new value and CAS it into
-- place without introducing new thunks or locking anything.  Note
-- that this is more STRICT than its standard counterpart and will
-- only place evaluated (WHNF) values in the IORef.
--
-- The upside is that sometimes we see a performance benefit.  The
-- downside is that this version is speculative -- when it retries, it
-- must reexecute the compution.
atomicModifyIORefCAS :: IORef a      -- ^ Mutable location to modify
                     -> (a -> (a,b)) -- ^ Computation runs one or more times (speculation)
                     -> IO b
atomicModifyIORefCAS = atomicModifyRefCAS

-- | A simpler version that modifies the state but does not return anything.
atomicModifyIORefCAS_ :: IORef t -> (t -> t) -> IO ()
atomicModifyIORefCAS_ = atomicModifyRefCAS_
