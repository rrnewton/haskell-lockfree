{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables, BangPatterns, CPP #-}
{-# LANGUAGE ForeignFunctionInterface, InstanceSigs, TypeFamilies #-}
module Data.Atomics.Class (MonadAtomic(..)) where

import Control.Monad.Primitive

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Primitive.Array (MutableArray(MutableArray))
import Data.Primitive.ByteArray (MutableByteArray(MutableByteArray))
import Data.Atomics.Internal

import Data.IORef
import Data.STRef
import GHC.IORef hiding (atomicModifyIORef)
import GHC.STRef
#if MIN_VERSION_base(4,7,0)
import GHC.Prim hiding ((==#))
import qualified GHC.PrimopWrappers as GPW
#else
import GHC.Prim
#endif
import GHC.Base (Int(I#))
import GHC.IO (IO(IO))
import GHC.ST (ST(ST))
-- import GHC.Word (Word(W#))


#if MIN_VERSION_base(4,8,0)
#else
import Data.Bits
import Data.Primitive.ByteArray (readByteArray)
#endif

{-# DEPRECATED fetchAddByteArrayInt "Replaced by fetchAddIntArray which returns the OLD value" #-}
class PrimMonad m => MonadAtomic m where
  type Ref m :: * -> *

  --------------------------------------------------------------------------------

  -- | Ordinary processor load instruction (non-atomic, not implying
  -- any memory barriers).
  --
  --   The difference between this function and `readIORef`, is that
  --   it returns a /ticket/, for use in future compare-and-swap
  --   operations.
  readForCAS :: Ref m a -> m (Ticket m a)

  -- | Performs a machine-level compare and swap (CAS) operation on an
  -- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when
  -- a swap is performed, along with the most 'current' value from the
  -- 'IORef'.  Note that this differs from the more common CAS
  -- behavior, which is to return the /old/ value before the CAS
  -- occured.
  --
  -- The reason for the difference is the ticket API.  This function
  -- always returns the ticket that you should use in your next CAS
  -- attempt.  In case of success, this ticket corresponds to the
  -- `new` value which you yourself installed in the `IORef`, whereas
  -- in the case of failure it represents the preexisting value
  -- currently in the IORef.
  --
  -- Note \"compare\" here means pointer equality in the sense of
  -- 'GHC.Prim.reallyUnsafePtrEquality#'.  However, the ticket API
  -- absolves the user of this module from needing to worry about the
  -- pointer equality of their values, which in general requires
  -- reasoning about the details of the Haskell implementation (GHC).
  --
  -- By convention this function is strict in the "new" value
  -- argument.  This isn't absolutely necesary, but we think it's a
  -- bad habit to use unevaluated thunks in this context.
  casRef :: Ref m a    -- ^ The 'IORef' containing a value 'current'
         -> Ticket m a -- ^ A ticket for the 'old' value
         -> a          -- ^ The 'new' value to replace 'current' if @old == current@
         -> m (Bool, Ticket m a) -- ^ Success flag, plus ticket for the NEXT operation.

  -- | This variant takes two tickets, i.e. the 'new' value is a
  -- ticket rather than an arbitrary, lifted, Haskell value.
  casRef2 :: Ref m a
           -> Ticket m a -- ^ A ticket for the 'old' value
           -> Ticket m a -- ^ A ticket for the 'new' value
           -> m (Bool, Ticket m a)

  -- | A drop-in replacement for `atomicModifyIORefCAS` that
  -- optimistically attempts to compute the new value and CAS it into
  -- place without introducing new thunks or locking anything.  Note
  -- that this is more STRICT than its standard counterpart and will
  -- only place evaluated (WHNF) values in the IORef.
  --
  -- The upside is that sometimes we see a performance benefit.  The
  -- downside is that this version is speculative -- when it retries,
  -- it must reexecute the compution.
  atomicModifyRefCAS :: Ref m a      -- ^ Mutable location to modify
                     -> (a -> (a,b)) -- ^ Computation runs one or more times (speculation)
                     -> m b

  -- | A simpler version that modifies the state but does not return anything.
  atomicModifyRefCAS_ :: Ref m t -> (t -> t) -> m ()
  atomicModifyRefCAS_ ref fn = atomicModifyRefCAS ref $ \a -> (fn a, ())

  --------------------------------------------------------------------------------

  -- | Like `readForCAS`, but for `MutVar#`.
  readMutVarForCAS :: MutVar# (PrimState m) a -> m ( Ticket m a )

  -- | MutVar counterpart of `casRef`.
  --
  --   By convention this is WHNF strict in the "new" value provided.
  casMutVar :: MutVar# (PrimState m) a -> Ticket m a -> a -> m (Bool, Ticket m a)
  casMutVar mv tick !new = casMutVar2 mv tick (seal new)

  -- | This variant takes two tickets, i.e. the 'new' value is a
  -- ticket rather than an arbitrary, lifted, Haskell value.
  casMutVar2 :: MutVar# (PrimState m) a -> Ticket m a -> Ticket m a -> m (Bool, Ticket m a)

  --------------------------------------------------------------------------------

  -- | Compare-and-swap.  Follows the same rules as `casIORef`,
  -- returning the ticket for then next operation.
  --
  --   By convention this is WHNF strict in the "new" value provided.
  casArrayElem  :: MutableArray (PrimState m) a -> Int -> Ticket m a -> a -> m (Bool, Ticket m a)
  casArrayElem arr i old !new = casArrayElem2 arr i old (seal new)

  -- | This variant takes two tickets: the 'new' value is a ticket
  -- rather than an arbitrary, lifted, Haskell value.
  casArrayElem2 :: MutableArray (PrimState m) a -> Int -> Ticket m a -> Ticket m a -> m (Bool, Ticket m a)

  -- | Ordinary processor load instruction (non-atomic, not implying any memory barriers).
  readArrayElem :: forall a . MutableArray (PrimState m) a -> Int -> m (Ticket m a)

  -- | Compare and swap on word-sized chunks of a byte-array.  For
  -- indexing purposes the bytearray is treated as an array of words
  -- (`Int`s).  Note that UNLIKE `casIORef` and `casArrayTicketed`,
  -- this does not need to operate on tickets.
  --
  -- Further, this version always returns the /old value/, that was
  -- read from the array during the CAS operation.  That is, it
  -- follows the normal protocol for CAS operations (and matches the
  -- underlying instruction on most architectures).
  --
  -- Implies a full memory barrier.
  casByteArrayInt :: MutableByteArray (PrimState m) -> Int -> Int -> Int -> m Int

  --------------------------------------------------------------------------------

  -- | Atomically add to a word of memory within a `MutableByteArray`,
  -- returning the value *before* the operation. Implies a full memory
  -- barrier.
  fetchAddIntArray :: MutableByteArray (PrimState m)
                   -> Int    -- ^ The offset into the array
                   -> Int    -- ^ The value to be added
                   -> m Int -- ^ The value *before* the addition

  -- | Atomically subtract to a word of memory within a
  -- `MutableByteArray`, returning the value *before* the
  -- operation. Implies a full memory barrier.
  fetchSubIntArray :: MutableByteArray (PrimState m)
                   -> Int    -- ^ The offset into the array
                   -> Int    -- ^ The value to be subtracted
                   -> m Int -- ^ The value *before* the addition

  -- | Atomically bitwise AND to a word of memory within a
  -- `MutableByteArray`, returning the value *before* the
  -- operation. Implies a full memory barrier.
  fetchAndIntArray :: MutableByteArray (PrimState m)
                   -> Int    -- ^ The offset into the array
                   -> Int    -- ^ The value to be AND-ed
                   -> m Int -- ^ The value *before* the addition

  -- | Atomically bitwise NAND to a word of memory within a
  -- `MutableByteArray`, returning the value *before* the
  -- operation. Implies a full memory barrier.
  fetchNandIntArray :: MutableByteArray (PrimState m)
                    -> Int    -- ^ The offset into the array
                    -> Int    -- ^ The value to be NAND-ed
                    -> m Int -- ^ The value *before* the addition

  -- | Atomically bitwise OR to a word of memory within a
  -- `MutableByteArray`, returning the value *before* the
  -- operation. Implies a full memory barrier.
  fetchOrIntArray :: MutableByteArray (PrimState m)
                  -> Int    -- ^ The offset into the array
                  -> Int    -- ^ The value to be OR-ed
                  -> m Int -- ^ The value *before* the addition

  -- | Atomically bitwise XOR to a word of memory within a
  -- `MutableByteArray`, returning the value *before* the
  -- operation. Implies a full memory barrier.
  fetchXorIntArray :: MutableByteArray (PrimState m)
                   -> Int    -- ^ The offset into the array
                   -> Int    -- ^ The value to be XOR-ed
                   -> m Int -- ^ The value *before* the addition

  -- | Atomically add to a word of memory within a `MutableByteArray`.
  --
  --   This function returns the NEW value of the location after the
  --   increment.  Thus, it is a bit misnamed, and in other contexts
  --   might be called "add-and-fetch", such as in GCC's
  --   `__sync_add_and_fetch`.
  fetchAddByteArrayInt ::  MutableByteArray (PrimState m) -> Int -> Int -> m Int

  --------------------------------------------------------------------------------

  -- | Memory barrier implemented by the GHC rts (see SMP.h).
  storeLoadBarrier :: m ()

  -- | Memory barrier implemented by the GHC rts (see SMP.h).
  loadLoadBarrier :: m ()

  -- | Memory barrier implemented by the GHC rts (see SMP.h).
  writeBarrier :: m ()

-- Concrete instances

-- | Because ST does not allow concurrency, barriers are no-ops here.
instance MonadAtomic (ST t) where
  type Ref (ST t) = STRef t

  --------------------------------------------------------------------------------

  readForCAS (STRef mv) = readMutVarForCAS mv
  casRef  (STRef var) old !new = casMutVar var old new
  casRef2 (STRef var) = casMutVar2 var

  atomicModifyRefCAS ref fn = do
    v <- readSTRef ref
    let (a, b) = fn v
    a `seq` writeSTRef ref a
    return b

  --------------------------------------------------------------------------------

  readMutVarForCAS = readMutVarForCASPrim ST
  casMutVar2 = casMutVar2Prim ST

  --------------------------------------------------------------------------------

  casArrayElem2   = casArrayElem2Prim   ST
  casByteArrayInt = casByteArrayIntPrim ST

  readArrayElem (MutableArray arr#) (I# i#) = ST $ \ st -> unsafeCoerce# (fn st)
    where
      fn = readArray# arr# i#

  --------------------------------------------------------------------------------

  fetchAddIntArray  = fetchAddIntArrayPrim  ST
  fetchSubIntArray  = fetchSubIntArrayPrim  ST
  fetchAndIntArray  = fetchAndIntArrayPrim  ST
  fetchNandIntArray = fetchNandIntArrayPrim ST
  fetchOrIntArray   = fetchOrIntArrayPrim   ST
  fetchXorIntArray  = fetchXorIntArrayPrim  ST
  fetchAddByteArrayInt = fetchAddByteArrayIntPrim ST

  --------------------------------------------------------------------------------

  storeLoadBarrier = return ()
  loadLoadBarrier  = return ()
  writeBarrier     = return ()

instance MonadAtomic IO where
  type Ref IO = IORef

  --------------------------------------------------------------------------------

  readForCAS (IORef (STRef mv)) = readMutVarForCAS mv
  casRef  (IORef (STRef var)) old !new = casMutVar var old new
  casRef2 (IORef (STRef var)) = casMutVar2 var

  atomicModifyRefCAS ref fn = do
     -- TODO: Should handle contention in a better way...
     tick <- readForCAS ref
     loop tick effort
    where
     effort = 30 :: Int -- TODO: Tune this.
     loop _   0     = atomicModifyIORef ref fn -- Fall back to the regular version.
     loop old tries = do
       (new,result) <- evaluate $ fn $ peekTicket old
       (b,tick) <- casRef ref old new
       if b
        then return result
        else loop tick (tries-1)

  -- atomicModifyRefCAS_ ref fn = atomicModifyRefCAS ref (\ x -> (fn
  -- x, ())) Can't inline a function with a loop so we duplicate this:
  -- <duplicated code>
  atomicModifyRefCAS_ ref fn = do
     tick <- readForCAS ref
     loop tick effort
    where
     effort = 30 :: Int -- TODO: Tune this.
     loop _   0     = atomicModifyIORef ref (\ x -> (fn x, ()))
     loop old tries = do
       new <- evaluate $ fn $ peekTicket old
       (b,val) <- casRef ref old new
       unless b $ loop val (tries-1)
-- </duplicated code>

  --------------------------------------------------------------------------------

  readMutVarForCAS = readMutVarForCASPrim IO
  casMutVar2 = casMutVar2Prim IO

  --------------------------------------------------------------------------------

  casArrayElem2   = casArrayElem2Prim   IO
  casByteArrayInt = casByteArrayIntPrim IO

  readArrayElem (MutableArray arr#) (I# i#) = IO $ \ st -> unsafeCoerce# (fn st)
    where
      fn = readArray# arr# i#

  --------------------------------------------------------------------------------

  fetchAddIntArray  = fetchAddIntArrayPrim  IO
  fetchSubIntArray  = fetchSubIntArrayPrim  IO
  fetchAndIntArray  = fetchAndIntArrayPrim  IO
  fetchNandIntArray = fetchNandIntArrayPrim IO
  fetchOrIntArray   = fetchOrIntArrayPrim   IO
  fetchXorIntArray  = fetchXorIntArrayPrim  IO
  fetchAddByteArrayInt = fetchAddByteArrayIntPrim IO

  --------------------------------------------------------------------------------

  storeLoadBarrier = storeLoadBarrierIO
  loadLoadBarrier  = loadLoadBarrierIO
  writeBarrier     = writeBarrierIO

-- GHC 7.8 changed some primops
#if MIN_VERSION_base(4,7,0)
(==#) :: Int# -> Int# -> Bool
(==#) x y = case x GPW.==# y of { 0# -> False; _ -> True }
#endif

-- Not exposing this for now.  Presently the idea is that you must read from the
-- mutable data structure itself to get a ticket.
seal :: a -> Ticket m a
seal = unsafeCoerce#

-- Internals for our fetch* family of functions, with CAS loop fallbacks for
-- GHC < 7.10:
{-# INLINE doAtomicRMW #-}
#if MIN_VERSION_base(4,8,0)
doAtomicRMW :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> (MutableByteArray# (PrimState m) -> Int# -> Int# -> State# (PrimState m) -> (# State# (PrimState m), Int# #)) --  primop
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int      --  exported function
doAtomicRMW inject atomicOp# (MutableByteArray mba#) (I# offset#) (I# val#) =
  inject $ \ s1# ->
    let (# s2#, res #) = atomicOp# mba# offset# val# s1# in
    (# s2#, I# res #)
#else
doAtomicRMW :: PrimMonad m => a
  -> (Int -> Int -> Int)                                     --  fallback op for CAS loop
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int      --  exported function
doAtomicRMW _ op mba offset val =
  let loop = do
        old <- readByteArray mba offset
        let !new = old `op` val
        actualOld <- casByteArrayInt mba offset old new
        if old == actualOld
            then return actualOld
            else loop
   in loop
{-# WARNING fetchSubIntArray "fetchSubIntArray is implemented with a CAS loop on GHC <7.10" #-}
{-# WARNING fetchAndIntArray "fetchAndIntArray is implemented with a CAS loop on GHC <7.10" #-}
{-# WARNING fetchNandIntArray "fetchNandIntArray is implemented with a CAS loop on GHC <7.10" #-}
{-# WARNING fetchOrIntArray "fetchOrIntArray is implemented with a CAS loop on GHC <7.10" #-}
{-# WARNING fetchXorIntArray "fetchXorIntArray is implemented with a CAS loop on GHC <7.10" #-}
#endif

-- GHC 7.8 consistently exposes these symbols while linking:

storeLoadBarrierIO, loadLoadBarrierIO, writeBarrierIO :: IO ()

#if MIN_VERSION_base(4,7,0) && !defined(mingw32_HOST_OS)

foreign import ccall  unsafe "store_load_barrier" storeLoadBarrierIO
  :: IO ()

foreign import ccall unsafe "load_load_barrier" loadLoadBarrierIO
  :: IO ()

foreign import ccall unsafe "write_barrier" writeBarrierIO
  :: IO ()

#else
-- GHC 7.6 did not consistently expose them (e.g. in the non-threaded RTS),
-- so rather we grab this functionality from RtsDup.c:
foreign import ccall  unsafe "DUP_store_load_barrier" storeLoadBarrierIO
  :: IO ()

foreign import ccall unsafe "DUP_load_load_barrier" loadLoadBarrierIO
  :: IO ()

foreign import ccall unsafe "DUP_write_barrier" writeBarrierIO
  :: IO ()
#endif

--------------------------------------------------------------------------------
-- Helper functions parametric over the underlying monad (IO or ST)

readMutVarForCASPrim :: ((State# s -> (# State# s, Ticket m a #)) -> t) -- monad
  -> MutVar# s a -> t
readMutVarForCASPrim inject mv = inject $ \st -> readForCAS# mv st

casMutVar2Prim :: ((State# s -> (# State# s, (Bool, Ticket m a) #)) -> t) -- monad
  -> MutVar# s a -> Ticket m a -> Ticket m a -> t
casMutVar2Prim inject mv tick new = inject $ \st ->
  case casMutVarTicketed# mv tick new st of
    (# st', flag, tick' #) -> (# st', (flag ==# 0#, tick') #)
--    (# st, if flag ==# 0# then Succeed tick' else Fail tick' #)
--    if flag ==# 0#    then       else (# st, Fail (W# tick')  #)

casArrayElem2Prim :: ((State# s -> (# State# s, (Bool, Ticket m a) #)) -> t) -- monad
  -> MutableArray s a -> Int -> Ticket m a -> Ticket m a -> t
casArrayElem2Prim inject (MutableArray arr#) (I# i#) old new = inject $ \s1# ->
  case casArrayTicketed# arr# i# old new s1# of 
    (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

casByteArrayIntPrim :: ((State# d -> (# State# d, Int #)) -> t) -- monad
  -> MutableByteArray d -> Int -> Int -> Int -> t
casByteArrayIntPrim inject (MutableByteArray mba#) (I# ix#) (I# old#) (I# new#) =
  inject $ \s1# -> case casIntArray# mba# ix# old# new# s1# of
    (# s2#, res #) -> (# s2#, I# res #)

fetchAddIntArrayPrim :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchAddIntArrayPrim inject (MutableByteArray mba#) (I# offset#) (I# incr#) = inject $ \s1# ->
  let (# s2#, res #) = fetchAddIntArray# mba# offset# incr# s1# in
  -- fetchAddIntArray# changed behavior in 7.10 to return the OLD
  -- value, so we need this to maintain backwards compatibility:
#if MIN_VERSION_base(4,8,0)
  (# s2#, I# res #)
#else
  (# s2#, I# (res -# incr#) #)
#endif

fetchSubIntArrayPrim :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchSubIntArrayPrim inject = doAtomicRMW inject
#if MIN_VERSION_base(4,8,0)
  fetchSubIntArray#
#else
  (-)
#endif

fetchAndIntArrayPrim :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchAndIntArrayPrim inject = doAtomicRMW inject
#if MIN_VERSION_base(4,8,0)
  fetchAndIntArray#
#else
  (.&.)
#endif

fetchNandIntArrayPrim :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchNandIntArrayPrim inject = doAtomicRMW inject
#if MIN_VERSION_base(4,8,0)
  fetchNandIntArray#
#else
  nand
   where nand x y = complement (x .&. y)
#endif

fetchOrIntArrayPrim :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchOrIntArrayPrim inject = doAtomicRMW inject
#if MIN_VERSION_base(4,8,0)
  fetchOrIntArray#
#else
  (.|.)
#endif

fetchXorIntArrayPrim :: PrimMonad m => ((State# (PrimState m) -> (# State# (PrimState m), Int #)) -> m Int) -- monad
  -> MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchXorIntArrayPrim inject = doAtomicRMW inject
#if MIN_VERSION_base(4,8,0)
  fetchXorIntArray#
#else
  xor
#endif

fetchAddByteArrayIntPrim :: ((State# s -> (# State# s, Int #)) -> a) -- monad
  -> MutableByteArray s -> Int -> Int -> a
fetchAddByteArrayIntPrim inject (MutableByteArray mba#) (I# offset#) (I# incr#) = inject $ \ s1# ->
  let (# s2#, res #) = fetchAddIntArray# mba# offset# incr# s1# in
  -- fetchAddIntArray# changed behavior in 7.10 to return the OLD
  -- value, so we need this to maintain forwards compatibility until
  -- removed:
#if MIN_VERSION_base(4,8,0)
  (# s2#, I# (res +# incr#) #)
#else
  (# s2#, I# res #)
#endif
