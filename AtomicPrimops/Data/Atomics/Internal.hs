{-# LANGUAGE CPP, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- | This module provides only the raw primops (and necessary types) for atomic
-- operations.  
module Data.Atomics.Internal 
   (casArray#, casByteArrayInt#, fetchAddByteArrayInt#, 
    readForCAS#, casMutVarTicketed#, 
    Ticket,
    stg_storeLoadBarrier#, stg_loadLoadBarrier#, stg_writeBarrier# )
  where 

import GHC.Base (Int(I#))
import GHC.Word (Word(W#))
import GHC.Prim (RealWorld, Int#, Word#, State#, MutableArray#, MutVar#,
                 MutableByteArray#, 
                 unsafeCoerce#, reallyUnsafePtrEquality#) 
#if MIN_VERSION_base(4,5,0)
-- Any is only in GHC 7.6!!!  We want 7.4 support.
import GHC.Prim (readMutVar#, casMutVar#, Any)
#else
#error "Need to figure out how to emulate Any () in GHC < 7.4 !"
-- type Any a = Word#
#endif    

#ifdef DEBUG_ATOMICS
{-# NOINLINE readForCAS# #-}
{-# NOINLINE casArray# #-}
{-# NOINLINE casMutVarTicketed# #-}
#define CASTFUN
#else
{-# INLINE casMutVarTicketed# #-}
{-# INLINE casArray# #-}
-- I *think* inlining may be ok here as long as casting happens on the arrow types:
#define CASTFUN
#endif

--------------------------------------------------------------------------------
-- CAS and friendsa
--------------------------------------------------------------------------------

-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArray# :: MutableArray# RealWorld a -> Int# -> Ticket a -> Ticket a 
          -> State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
#ifdef CASTFUN
-- WARNING: cast of a function -- need to verify these are safe or eta expand.
casArray# = unsafeCoerce# casArrayTypeErased#
#endif

-- | When performing compare-and-swaps, the /ticket/ encapsulates proof
-- that a thread observed a specific previous value of a mutable
-- variable.  It is provided in lieu of the "old" value to
-- compare-and-swap.
type Ticket a = Any a
-- If we allow tickets to be a pointer type, then the garbage collector will update
-- the pointer when the object moves.

#if 0
-- This technique is UNSAFE.  False negatives are tolerable, but it may also
-- introduce the possibility of false positives.
type Ticket = Word
type Ticket# = Word# 
#endif

instance Show (Ticket a) where
  show _ = "<CAS_ticket>"


{-# NOINLINE ptrEq #-}
ptrEq :: a -> a -> Bool
ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1

instance Eq (Ticket a) where
  (==) = ptrEq

--------------------------------------------------------------------------------

readForCAS# :: MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, Ticket a #)
-- WARNING: cast of a function -- need to verify these are safe or eta expand:
#ifdef CASTFUN
readForCAS# = unsafeCoerce# readMutVar#
#else
readForCAS# mv rw =
  case readMutVar# mv rw of
    (# rw', a #) -> (# rw', unsafeCoerce# a #)
#endif


casMutVarTicketed# :: MutVar# RealWorld a -> Ticket a -> Ticket a ->
               State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
-- WARNING: cast of a function -- need to verify these are safe or eta expand:
#ifdef CASTFUN
casMutVarTicketed# = unsafeCoerce# casMutVar_TypeErased#
#endif

--------------------------------------------------------------------------------
-- Memory barriers
--------------------------------------------------------------------------------

foreign import prim "stg_store_load_barrier" stg_storeLoadBarrier#
  :: State# RealWorld -> (# State# RealWorld, Int# #)

foreign import prim "stg_load_load_barrier" stg_loadLoadBarrier#
  :: State# RealWorld -> (# State# RealWorld, Int# #)

foreign import prim "stg_write_barrier" stg_writeBarrier#
  :: State# RealWorld -> (# State# RealWorld, Int# #)

--------------------------------------------------------------------------------
-- Type-erased versions that call the raw foreign primops:
--------------------------------------------------------------------------------
-- Due to limitations of the "foreign import prim" mechanism, we can't use the
-- polymorphic signature for the below functions.  So we lie to the type system
-- instead.

foreign import prim "stg_casArrayzh" casArrayTypeErased#
  :: MutableArray# RealWorld () -> Int# -> Any () -> Any () -> 
     State# RealWorld  -> (# State# RealWorld, Int#, Any () #) 
--   out_of_line = True
--   has_side_effects = True

-- | This alternate version of casMutVar returns an opaque "ticket" for
--   future CAS operations.
foreign import prim "stg_casMutVar2zh" casMutVar_TypeErased#
  :: MutVar# RealWorld () -> Any () -> Any () ->
     State# RealWorld -> (# State# RealWorld, Int#, Any () #)

-- foreign import prim "stg_readMutVar2zh" readMutVar_TypeErased#
--   :: MutVar# RealWorld () -> 
--      State# RealWorld -> (# State# RealWorld, Any () #)

foreign import prim "stg_casByteArrayIntzh" casByteArrayInt#
  :: MutableByteArray# s -> Int# -> Int# -> Int# ->
     State# s -> (# State# s, Int# #) 

foreign import prim "stg_fetchAddByteArrayIntzh" fetchAddByteArrayInt#
  :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #) 
