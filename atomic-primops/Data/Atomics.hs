{-# LANGUAGE  MagicHash, UnboxedTuples, ScopedTypeVariables, BangPatterns, CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

   -- * Atomic operations on IORefs
   readForCAS, casIORef, casIORef2,
   atomicModifyIORefCAS, atomicModifyIORefCAS_,

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
   fetchAddByteArrayInt
 ) where

import Control.Exception (evaluate)
import Data.Primitive.Array (MutableArray(MutableArray))
import Data.Primitive.ByteArray (MutableByteArray(MutableByteArray))
import Data.Atomics.Internal

import Data.IORef
import GHC.IORef (IORef (..))
import GHC.STRef
import GHC.Exts hiding ((==#))
import qualified GHC.PrimopWrappers as GPW
import GHC.IO (IO(IO))
-- import GHC.Word (Word(W#))

#ifdef DEBUG_ATOMICS
#warning "Activating DEBUG_ATOMICS... NOINLINE's and more"
{-# NOINLINE casIORef #-}
{-# NOINLINE casArrayElem2 #-}
{-# NOINLINE readArrayElem #-}
{-# NOINLINE readForCAS #-}
{-# NOINLINE casArrayElem #-}
{-# NOINLINE casIORef2 #-}
{-# NOINLINE readMutVarForCAS #-}
{-# NOINLINE casMutVar #-}
{-# NOINLINE casMutVar2 #-}
{-# NOINLINE casByteArrayInt #-}
{-# NOINLINE fetchAddIntArray #-}
{-# NOINLINE fetchSubIntArray #-}
{-# NOINLINE fetchAndIntArray #-}
{-# NOINLINE fetchNandIntArray #-}
{-# NOINLINE fetchOrIntArray #-}
{-# NOINLINE fetchXorIntArray #-}
#else
{-# INLINE casIORef #-}
{-# INLINE casArrayElem2 #-}
{-# INLINE readArrayElem #-}
{-# INLINE readForCAS #-}
{-# INLINE casArrayElem #-}
{-# INLINE casIORef2 #-}
{-# INLINE readMutVarForCAS #-}
{-# INLINE casMutVar #-}
{-# INLINE casMutVar2 #-}
{-# INLINE fetchAddIntArray #-}
{-# INLINE fetchSubIntArray #-}
{-# INLINE fetchAndIntArray #-}
{-# INLINE fetchNandIntArray #-}
{-# INLINE fetchOrIntArray #-}
{-# INLINE fetchXorIntArray #-}
#endif


-- GHC 7.8 changed some primops
(==#) :: Int# -> Int# -> Bool
(==#) x y = case x GPW.==# y of { 0# -> False; _ -> True }

--------------------------------------------------------------------------------

-- | Compare-and-swap.  Follows the same rules as `casIORef`, returning the ticket for
--   then next operation.
--
--   By convention this is WHNF strict in the "new" value provided.
casArrayElem :: MutableArray RealWorld a -> Int -> Ticket a -> a -> IO (Bool, Ticket a)
-- casArrayElem (MutableArray arr#) (I# i#) old new = IO$ \s1# ->
--  case casArray# arr# i# old new s1# of
--    (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)
casArrayElem arr i old !new = casArrayElem2 arr i old (seal new)

-- | This variant takes two tickets: the 'new' value is a ticket rather than an
-- arbitrary, lifted, Haskell value.
casArrayElem2 :: MutableArray RealWorld a -> Int -> Ticket a -> Ticket a -> IO (Bool, Ticket a)
casArrayElem2 (MutableArray arr#) (I# i#) old new = IO$ \s1# ->
 case casArrayTicketed# arr# i# old new s1# of
   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

-- | Ordinary processor load instruction (non-atomic, not implying any memory barriers).
readArrayElem :: forall a . MutableArray RealWorld a -> Int -> IO (Ticket a)
readArrayElem (MutableArray arr#) (I# i#) = IO $ \ st ->
  readArrayElem# arr# i# st

-- | Compare and swap on word-sized chunks of a byte-array.  For indexing purposes
-- the bytearray is treated as an array of words (`Int`s).  Note that UNLIKE
-- `casIORef` and `casArrayTicketed`, this does not need to operate on tickets.
--
-- Further, this version always returns the /old value/, that was read from the array during
-- the CAS operation.  That is, it follows the normal protocol for CAS operations
-- (and matches the underlying instruction on most architectures).
--
-- Implies a full memory barrier.
casByteArrayInt ::  MutableByteArray RealWorld -> Int -> Int -> Int -> IO Int
casByteArrayInt (MutableByteArray mba#) (I# ix#) (I# old#) (I# new#) =
  IO$ \s1# ->
  -- It would be nice to avoid allocating a tuple result here.
  -- Further, it will probably not be possible or the compiler to unbox the integer
  -- result either with the current arrangement:

  -- case casByteArrayInt# mba# ix# old# new# s1# of
  --   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, I# res) #)

  let !(# s2#, res #) = casIntArray# mba# ix# old# new# s1# in
  (# s2#, (I# res) #)
  -- I don't know if a let will mak any difference here... hopefully not.


--------------------------------------------------------------------------------
-- Fetch-and-* family of functions:

-- | Atomically add to a word of memory within a `MutableByteArray`, returning
-- the value *before* the operation. Implies a full memory barrier.
fetchAddIntArray :: MutableByteArray RealWorld
                     -> Int    -- ^ The offset into the array
                     -> Int    -- ^ The value to be added
                     -> IO Int -- ^ The value *before* the addition
fetchAddIntArray (MutableByteArray mba#) (I# offset#) (I# incr#) = IO $ \ s1# ->
  let !(# s2#, res #) = fetchAddIntArray# mba# offset# incr# s1# in
  (# s2#, (I# res) #)


-- | Atomically subtract to a word of memory within a `MutableByteArray`,
-- returning the value *before* the operation. Implies a full memory barrier.
fetchSubIntArray :: MutableByteArray RealWorld
                     -> Int    -- ^ The offset into the array
                     -> Int    -- ^ The value to be subtracted
                     -> IO Int -- ^ The value *before* the addition
fetchSubIntArray = doAtomicRMW fetchSubIntArray#

-- | Atomically bitwise AND to a word of memory within a `MutableByteArray`,
-- returning the value *before* the operation. Implies a full memory barrier.
fetchAndIntArray :: MutableByteArray RealWorld
                     -> Int    -- ^ The offset into the array
                     -> Int    -- ^ The value to be AND-ed
                     -> IO Int -- ^ The value *before* the addition
fetchAndIntArray = doAtomicRMW fetchAndIntArray#

-- | Atomically bitwise NAND to a word of memory within a `MutableByteArray`,
-- returning the value *before* the operation. Implies a full memory barrier.
fetchNandIntArray :: MutableByteArray RealWorld
                     -> Int    -- ^ The offset into the array
                     -> Int    -- ^ The value to be NAND-ed
                     -> IO Int -- ^ The value *before* the addition
fetchNandIntArray = doAtomicRMW fetchNandIntArray#

-- | Atomically bitwise OR to a word of memory within a `MutableByteArray`,
-- returning the value *before* the operation. Implies a full memory barrier.
fetchOrIntArray :: MutableByteArray RealWorld
                     -> Int    -- ^ The offset into the array
                     -> Int    -- ^ The value to be OR-ed
                     -> IO Int -- ^ The value *before* the addition
fetchOrIntArray = doAtomicRMW fetchOrIntArray#

-- | Atomically bitwise XOR to a word of memory within a `MutableByteArray`,
-- returning the value *before* the operation. Implies a full memory barrier.
fetchXorIntArray :: MutableByteArray RealWorld
                     -> Int    -- ^ The offset into the array
                     -> Int    -- ^ The value to be XOR-ed
                     -> IO Int -- ^ The value *before* the addition
fetchXorIntArray = doAtomicRMW fetchXorIntArray#


-- Internals for our fetch* family of functions, with CAS loop fallbacks for
-- GHC < 7.10:
{-# INLINE doAtomicRMW #-}
doAtomicRMW :: (MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)) --  primop
            -> MutableByteArray RealWorld -> Int -> Int -> IO Int      --  exported function
doAtomicRMW atomicOp# =
  \(MutableByteArray mba#) (I# offset#) (I# val#) ->
    IO $ \ s1# ->
      let !(# s2#, res #) = atomicOp# mba# offset# val# s1# in
      (# s2#, (I# res) #)


{-# DEPRECATED fetchAddByteArrayInt "Replaced by fetchAddIntArray which returns the OLD value" #-}
-- | Atomically add to a word of memory within a `MutableByteArray`.
--
--   This function returns the NEW value of the location after the increment.
--   Thus, it is a bit misnamed, and in other contexts might be called "add-and-fetch",
--   such as in GCC's `__sync_add_and_fetch`.
fetchAddByteArrayInt ::  MutableByteArray RealWorld -> Int -> Int -> IO Int
fetchAddByteArrayInt (MutableByteArray mba#) (I# offset#) (I# incr#) = IO $ \ s1# ->
  let !(# s2#, res #) = fetchAddIntArray# mba# offset# incr# s1# in
  (# s2#, (I# (res +# incr#)) #)


--------------------------------------------------------------------------------
{- WIP. Having trouble writing good tests for these, and not sure how useful
 - these are. See #43 discussion
 -
 - Also remember to add these to the INLINE / NOINLINE section when exported



-- | Given an array and an offset in Int units, read an element. The index is
-- assumed to be in bounds. Implies a full memory barrier.
atomicReadIntArray :: MutableByteArray RealWorld -> Int -> IO Int
atomicReadIntArray (MutableByteArray mba#) (I# ix#) = IO $ \ s# ->
    case atomicReadIntArray# mba# ix# s# of
        (# s2#, n# #) -> (# s2#, I# n# #)

-- | Given an array and an offset in Int units, write an element. The index is
-- assumed to be in bounds. Implies a full memory barrier.
atomicWriteIntArray :: MutableByteArray RealWorld -> Int -> Int -> IO ()
atomicWriteIntArray (MutableByteArray mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case atomicWriteIntArray# mba# ix# n# s# of
        s2# -> (# s2#, () #)

-}

--------------------------------------------------------------------------------

-- | Ordinary processor load instruction (non-atomic, not implying any memory barriers).
--
--   The difference between this function and `readIORef`, is that it returns a /ticket/,
--   for use in future compare-and-swap operations.
readForCAS :: IORef a -> IO ( Ticket a )
readForCAS (IORef (STRef mv)) = readMutVarForCAS mv

-- | Performs a machine-level compare and swap (CAS) operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the most 'current' value from the 'IORef'.
-- Note that this differs from the more common CAS behavior, which is to
-- return the /old/ value before the CAS occured.
--
-- The reason for the difference is the ticket API.  This function always returns the
-- ticket that you should use in your next CAS attempt.  In case of success, this ticket
-- corresponds to the `new` value which you yourself installed in the `IORef`, whereas
-- in the case of failure it represents the preexisting value currently in the IORef.
--
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.  However, the ticket API absolves
-- the user of this module from needing to worry about the pointer equality of their
-- values, which in general requires reasoning about the details of the Haskell
-- implementation (GHC).
--
-- By convention this function is strict in the "new" value argument.  This isn't
-- absolutely necesary, but we think it's a bad habit to use unevaluated thunks in
-- this context.
casIORef :: IORef a  -- ^ The 'IORef' containing a value 'current'
         -> Ticket a -- ^ A ticket for the 'old' value
         -> a        -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (Bool, Ticket a) -- ^ Success flag, plus ticket for the NEXT operation.
casIORef (IORef (STRef var)) old !new = casMutVar var old new

-- | This variant takes two tickets, i.e. the 'new' value is a ticket rather than an
-- arbitrary, lifted, Haskell value.
casIORef2 :: IORef a
         -> Ticket a -- ^ A ticket for the 'old' value
         -> Ticket a -- ^ A ticket for the 'new' value
         -> IO (Bool, Ticket a)
casIORef2 (IORef (STRef var)) old new = casMutVar2 var old new


--------------------------------------------------------------------------------

-- | Like `readForCAS`, but for `MutVar#`.
readMutVarForCAS :: MutVar# RealWorld a -> IO ( Ticket a )
readMutVarForCAS mv = IO$ \ st -> readForCAS# mv st

-- | MutVar counterpart of `casIORef`.
--
--   By convention this is WHNF strict in the "new" value provided.
casMutVar :: MutVar# RealWorld a -> Ticket a -> a -> IO (Bool, Ticket a)
casMutVar mv tick !new =
  -- trace ("TEMPDBG: Inside casMutVar.. ") $
  casMutVar2 mv tick (seal new)

-- | This variant takes two tickets, i.e. the 'new' value is a ticket rather than an
-- arbitrary, lifted, Haskell value.
casMutVar2 :: MutVar# RealWorld a -> Ticket a -> Ticket a -> IO (Bool, Ticket a)
casMutVar2 mv tick new = IO$ \st ->
  case casMutVarTicketed# mv tick new st of
    (# st', flag, tick' #) ->
      (# st', (flag ==# 0#, tick') #)
--      (# st, if flag ==# 0# then Succeed tick' else Fail tick' #)
--      if flag ==# 0#    then       else (# st, Fail (W# tick')  #)

--------------------------------------------------------------------------------
-- Memory barriers
--------------------------------------------------------------------------------

-- | Memory barrier implemented by the GHC rts (see SMP.h).
-- storeLoadBarrier :: IO ()

-- | Memory barrier implemented by the GHC rts (see SMP.h).
-- loadLoadBarrier :: IO ()

-- | Memory barrier implemented by the GHC rts (see SMP.h).
-- writeBarrier :: IO ()

#if !(defined(mingw32_HOST_OS) && __GLASGOW_HASKELL__ < 802)
-- | Memory barrier implemented by the GHC rts (see SMP.h).
foreign import ccall  unsafe "store_load_barrier" storeLoadBarrier
  :: IO ()

-- | Memory barrier implemented by the GHC rts (see SMP.h).
foreign import ccall unsafe "load_load_barrier" loadLoadBarrier
  :: IO ()

-- | Memory barrier implemented by the GHC rts (see SMP.h).
foreign import ccall unsafe "write_barrier" writeBarrier
  :: IO ()
#else
#warning "importing store_load_barrier and friends from the package's C code."

-- Workaround for Trac #12846, which affects old GHCs on Windows
foreign import ccall  unsafe "DUP_store_load_barrier" storeLoadBarrier
  :: IO ()

foreign import ccall unsafe "DUP_load_load_barrier" loadLoadBarrier
  :: IO ()

foreign import ccall unsafe "DUP_write_barrier" writeBarrier
  :: IO ()
#endif

--------------------------------------------------------------------------------


-- | A drop-in replacement for `atomicModifyIORef` that
--   optimistically attempts to compute the new value and CAS it into
--   place without introducing new thunks or locking anything.  Note
--   that this is more STRICT than its standard counterpart and will only
--   place evaluated (WHNF) values in the IORef.
--
--   The upside is that sometimes we see a performance benefit.
--   The downside is that this version is speculative -- when it
--   retries, it must reexecute the compution.
atomicModifyIORefCAS :: IORef a      -- ^ Mutable location to modify
                     -> (a -> (a,b)) -- ^ Computation runs one or more times (speculation)
                     -> IO b
atomicModifyIORefCAS ref fn = do
   -- TODO: Should handle contention in a better way...
   tick <- readForCAS ref
   loop tick effort
  where
   effort = 30 :: Int -- TODO: Tune this.
   loop _   0     = atomicModifyIORef ref fn -- Fall back to the regular version.
   loop old tries = do
     (new,result) <- evaluate $ fn $ peekTicket old
     (b,tick) <- casIORef ref old new
     if b
      then return result
      else loop tick (tries-1)


-- | A simpler version that modifies the state but does not return anything.
atomicModifyIORefCAS_ :: IORef t -> (t -> t) -> IO ()
-- atomicModifyIORefCAS_ ref fn = atomicModifyIORefCAS ref (\ x -> (fn x, ()))
-- Can't inline a function with a loop so we duplicate this:
-- <duplicated code>
atomicModifyIORefCAS_ ref fn = do
   tick <- readForCAS ref
   loop tick effort
  where
   effort = 30 :: Int -- TODO: Tune this.
   loop _   0     = atomicModifyIORef ref (\ x -> (fn x, ()))
   loop old tries = do
     new <- evaluate $ fn $ peekTicket old
     (b,val) <- casIORef ref old new
     if b
      then return ()
      else loop val (tries-1)
-- </duplicated code>
