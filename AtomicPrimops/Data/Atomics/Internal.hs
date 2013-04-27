{-# LANGUAGE CPP, TypeSynonymInstances #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

module Data.Atomics.Internal 
   (casArray#, 
    readForCAS#, casMutVarTicketed#, 
    Ticket )
  where 

import GHC.Base (Int(I#))
import GHC.Word (Word(W#))
import GHC.Prim (RealWorld, Int#, Word#, State#, MutableArray#, unsafeCoerce#, MutVar#) 
#if MIN_VERSION_base(4,6,0)
-- Any is only in GHC 7.6!!!  We want 7.4 support.
import GHC.Prim (readMutVar#, casMutVar#, Any)
#else
#error "Need to figure out how to emulate Any () in GHC 7.4."
-- type Any a = Word#
#endif    

--------------------------------------------------------------------------------
-- Entrypoints for end-users
--------------------------------------------------------------------------------

{-# INLINE casArray# #-}
-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArray# :: MutableArray# RealWorld a -> Int# -> a -> a 
          -> State# RealWorld -> (# State# RealWorld, Int#, a #)    
casArray# = unsafeCoerce# casArrayTypeErased#


-- | When performing compare-and-swaps, the /ticket/ encapsulates proof
-- that a thread observed a specific previous value of a mutable
-- variable.  It is provided in lieu of the "old" value to
-- compare-and-swap.
type Ticket a = Any a
-- If we allow tickets to be a pointer type, then the garbage collector will update
-- the pointer when the object moves.

type Ticket# = Word# 

#if 0
-- This technique is UNSAFE.  False negatives are tolerable, but it may also
-- introduce the possibility of false positives.
type Ticket = Word
type Ticket# = Word# 
#endif

instance Show (Ticket a) where
  show _ = "<CAS_ticket>"


{-# INLINE readForCAS# #-}
readForCAS# :: MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, Ticket a #)
readForCAS# = unsafeCoerce# readMutVar#
-- readForCAS# = unsafeCoerce# readMutVar_TypeErased#

{-# INLINE casMutVarTicketed# #-}
casMutVarTicketed# :: MutVar# RealWorld a -> Ticket a -> a ->
               State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
casMutVarTicketed# = unsafeCoerce# casMutVar_TypeErased#
-- casMutVarTicketed# = unsafeCoerce# casMutVar#

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

-- | This alternate version of casMutVar returns a numeric "ticket" for
--   future CAS operations.
foreign import prim "stg_casMutVar2zh" casMutVar_TypeErased#
  :: MutVar# RealWorld () -> Any () -> Any () ->
     State# RealWorld -> (# State# RealWorld, Int#, Any () #)

-- foreign import prim "stg_readMutVar2zh" readMutVar_TypeErased#
--   :: MutVar# RealWorld () -> 
--      State# RealWorld -> (# State# RealWorld, Any () #)
