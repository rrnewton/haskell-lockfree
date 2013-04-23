{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

module Data.Atomics.Internal 
   (casArray#, 
    readForCAS#, casMutVar2#, 
    Ticket, Ticket#)
  where 
import GHC.Prim
import GHC.Base (Int(I#))
import GHC.Word (Word(W#))

--------------------------------------------------------------------------------
-- Entrypoints for end-users
--------------------------------------------------------------------------------

{-# INLINE casArray# #-}
-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArray# :: MutableArray# RealWorld a -> Int# -> a -> a 
          -> State# RealWorld -> (# State# RealWorld, Int#, a #)    
casArray# = unsafeCoerce# casArrayTypeErased#


-- | When performing compare-and-swaps, the /ticket/ captures proof
-- that a thread observed a specific previous value of a mutable
-- variable.  It is provided in lieu of the "old" value to
-- compare-and-swap.
type Ticket = Word
type Ticket# = Word# 


type STRep2 a b   = State# RealWorld -> (# State# RealWorld, a, b #)
type STRep3 a b c = State# RealWorld -> (# State# RealWorld, a, b, c #)
-- readForCAS# :: MutVar# RealWorld a -> STRep2 Ticket# a

readForCAS# :: MutVar# RealWorld a -> State# RealWorld -> (# State# RealWorld, Ticket#, a #)
readForCAS# = unsafeCoerce# readMutVar_TypeErased#


-- readForCAS# :: MutVar# RealWorld a -> State# RealWorld -> (# State# RealWorld, Ticket#, a #)
-- readForCAS# = unsafeCoerce# readMutVar_TypeErased
casMutVar2# :: MutVar# RealWorld a -> Ticket# -> a ->
               State# RealWorld -> (# State# RealWorld, Int#, Ticket#, a #)
casMutVar2# = unsafeCoerce# casMutVar_TypeErased#


--------------------------------------------------------------------------------
-- Type-erased versions that call the raw foreign primops:
--------------------------------------------------------------------------------
-- Due to limitations of the "foreign import prim" mechanism, we can't use the
-- polymorphic signature for the below functions.  So we lie to the type system
-- instead.

-- type TheValType = Any () -- This type only works in GHC 7.6!!!  We want 7.4 support.
type TheValType = Word#

foreign import prim "stg_casArrayzh" casArrayTypeErased#
  :: MutableArray# RealWorld () -> Int# -> TheValType -> TheValType -> 
     State# RealWorld  -> (# State# RealWorld, Int#, TheValType #) 
--   out_of_line = True
--   has_side_effects = True

-- | This alternate version of casMutVar returns a numeric "ticket" for
--   future CAS operations.
foreign import prim "stg_casMutVar2zh" casMutVar_TypeErased#
  :: MutVar# RealWorld () -> Ticket# -> TheValType ->
     State# RealWorld -> (# State# RealWorld, Int#, Ticket#, TheValType #)


foreign import prim "stg_readMutVar2zh" readMutVar_TypeErased#
  :: MutVar# RealWorld () -> 
     State# RealWorld -> (# State# RealWorld, Ticket#, TheValType #)

     
