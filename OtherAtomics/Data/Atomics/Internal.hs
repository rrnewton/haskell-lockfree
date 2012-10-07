{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

module Data.Atomics.Internal (casArray# ) where 
import GHC.Prim
import GHC.Base (Int(I#))

{-# INLINE casArray# #-}
-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArray# :: MutableArray# RealWorld a -> Int# -> a -> a -> State# RealWorld -> (# State# RealWorld, Int#, a #)    
casArray# = unsafeCoerce# casArrayTypeErased#

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
