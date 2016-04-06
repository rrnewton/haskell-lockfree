{-# LANGUAGE CPP, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

#define CASTFUN

-- | This module provides only the raw primops (and necessary types) for atomic
-- operations.  
module Data.Atomics.Internal 
   (
    casIntArray#, fetchAddIntArray#, 
    readForCAS#, casMutVarTicketed#, casArrayTicketed#, 
    Ticket,
    -- * Very unsafe, not to be used
    ptrEq
   )
  where 

import GHC.Base (Int(I#), Any)
import GHC.Prim (RealWorld, Int#, State#, MutableArray#, MutVar#,
                 unsafeCoerce#, reallyUnsafePtrEquality#) 

#if MIN_VERSION_base(4,7,0)
import GHC.Prim (casArray#, casIntArray#, fetchAddIntArray#, readMutVar#, casMutVar#)
#elif MIN_VERSION_base(4,6,0)
-- Any is only supported in the FFI in the way we need in GHC 7.6+
import GHC.Prim (readMutVar#, MutableByteArray#)
#else
#error "Need to figure out how to emulate Any () in GHC <= 7.4 !"
-- import GHC.Prim (Word#)
-- type Any a = Word#
#endif    

#ifdef DEBUG_ATOMICS
{-# NOINLINE readForCAS# #-}
{-# NOINLINE casMutVarTicketed# #-}
{-# NOINLINE casArrayTicketed# #-}
#else
-- {-# INLINE casMutVarTicketed# #-}
{-# INLINE casArrayTicketed# #-}
-- I *think* inlining may be ok here as long as casting happens on the arrow types:
#endif

--------------------------------------------------------------------------------
-- CAS and friends
--------------------------------------------------------------------------------

-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArrayTicketed# :: MutableArray# RealWorld a -> Int# -> Ticket a -> Ticket a 
          -> State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
-- WARNING: cast of a function -- need to verify these are safe or eta expand.
casArrayTicketed# = unsafeCoerce#
#if MIN_VERSION_base(4,7,0)
   -- In GHC 7.8 onward we just want to expose the existing primop with a different type:
   casArray#
#else
   casArrayTypeErased#
#endif
    
-- | When performing compare-and-swaps, the /ticket/ encapsulates proof
-- that a thread observed a specific previous value of a mutable
-- variable.  It is provided in lieu of the "old" value to
-- compare-and-swap.
--
-- Design note: `Ticket`s exist to hide objects from the GHC compiler, which
-- can normally perform many optimizations that change pointer equality.  A Ticket,
-- on the other hand, is a first-class object that can be handled by the user,
-- but will not have its pointer identity changed by compiler optimizations
-- (but will of course, change addresses during garbage collection).
newtype Ticket a = Ticket Any
-- If we allow tickets to be a pointer type, then the garbage collector will update
-- the pointer when the object moves.

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
casMutVarTicketed# =
#if MIN_VERSION_base(4,7,0) 
  unsafeCoerce# casMutVar#
#else
  unsafeCoerce# casMutVar_TypeErased#
#endif

--------------------------------------------------------------------------------
-- Type-erased versions that call the raw foreign primops:
--------------------------------------------------------------------------------
-- Due to limitations of the "foreign import prim" mechanism, we can't use the
-- polymorphic signature for the below functions.  So we lie to the type system
-- instead.

#if MIN_VERSION_base(4,7,0) 
#else

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
  -- with has_side_effects = True
  --      commutable = False

foreign import prim "stg_casByteArrayIntzh" casIntArray#
  :: MutableByteArray# s -> Int# -> Int# -> Int# ->
     State# s -> (# State# s, Int# #) 

foreign import prim "stg_fetchAddByteArrayIntzh" fetchAddIntArray#
  :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #) 

#endif
