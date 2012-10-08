{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

module Data.Atomics.Internal 
       (casArray#, readForCAS#, Ticket, Ticket#)
       where 
import GHC.Prim
import GHC.Base (Int(I#))
import GHC.Word (Word(W#))

--------------------------------------------------------------------------------
-- Entrypoints for end-users
--------------------------------------------------------------------------------

{-# INLINE casArray# #-}
-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArray# :: MutableArray# RealWorld a -> Int# -> a -> a -> State# RealWorld -> (# State# RealWorld, Int#, a #)    
casArray# = unsafeCoerce# casArrayTypeErased#


-- | When performing compare-and-swaps, the /ticket/ captures proof
-- that a thread observed a specific previous value of a mutable
-- variable.  It is provided in lieu of the "old" value to
-- compare-and-swap.
type Ticket = Word
type Ticket# = Word# 

readForCAS# :: MutVar# RealWorld a -> State# RealWorld -> (# State# RealWorld, Ticket#, a #)
readForCAS# = unsafeCoerce# readMutVar_TypeErased

--------------------------------------------------------------------------------
-- Type-erased versions that call the raw foreign primops:
--------------------------------------------------------------------------------

-- type Dummy = Any () -- The above type only works in GHC 7.6!!!  We want 7.4 support.
type Dummy = Word#

-- Due to limitations of the "foreign import prim" mechanism, we can't
-- use the polymorphic signature for this function.  So we lie to the
-- type system here.
foreign import prim "stg_casArrayzh" casArrayTypeErased#
  :: MutableArray# RealWorld () -> Int# -> Dummy -> Dummy -> 
     State# RealWorld  -> (# State# RealWorld, Int#, Dummy #) 

--   out_of_line = True
--   has_side_effects = True



-- This alternate version of casMutVar returns a numeric "ticket" for
-- future CAS operations.
foreign import prim "stg_casArray2zh" casMutVar2#
  :: MutVar# RealWorld () -> Word# -> Dummy ->
     State# RealWorld -> (# State# RealWorld, Int#, Word#, Dummy #)


foreign import prim "stg_readMutVar2zh" readMutVar_TypeErased
  :: MutVar# RealWorld () -> 
     State# RealWorld -> (# State# RealWorld, Word#, Dummy #)

     
