{-# LANGUAGE CPP, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- | This module provides only the raw primops (and necessary types) for atomic
-- operations.  
module Data.Atomics.Internal 
   (casArray#, 
    readForCAS#, casMutVarTicketed#, 
    Ticket,

    fetch_and_add_word
   )
  where 

import GHC.Base (Int(I#))
import GHC.Word (Word(W#))
import GHC.Prim (RealWorld, Int#, Word#, State#, MutableArray#, unsafeCoerce#, MutVar#, reallyUnsafePtrEquality#)
import Foreign.Ptr (Ptr)
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
casArray# :: MutableArray# RealWorld a -> Int# -> Ticket a -> Ticket a 
          -> State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
casArray# = unsafeCoerce# casArrayTypeErased#


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


{-# INLINE readForCAS# #-}
readForCAS# :: MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, Ticket a #)
readForCAS# = unsafeCoerce# readMutVar#
-- readForCAS# = unsafeCoerce# readMutVar_TypeErased#

{-# INLINE casMutVarTicketed# #-}
casMutVarTicketed# :: MutVar# RealWorld a -> Ticket a -> Ticket a ->
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


--------------------------------------------------------------------------------

foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_add_word"
    fetch_and_add_word :: Ptr Word -> Word -> IO Word
{-
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_sub_word"
    fetch_and_sub_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_or_word"
    fetch_and_or_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_and_word"
    fetch_and_and_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_xor_word"
    fetch_and_xor_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h fetch_and_nand_word"
    fetch_and_nand_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h add_and_fetch_word"
    add_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h sub_and_fetch_word"
    sub_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h or_and_fetch_word"
    or_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h and_and_fetch_word"
    and_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h xor_and_fetch_word"
    xor_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h nand_and_fetch_word"
    nand_and_fetch_word :: Ptr Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h bool_compare_and_swap_word"
    bool_compare_and_swap_word :: Ptr Word -> Word -> Word -> IO Bool
foreign import ccall unsafe "atomic-bitops-gcc.h val_compare_and_swap_word"
    val_compare_and_swap_word :: Ptr Word -> Word -> Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h lock_test_and_set_word"
    lock_test_and_set_word :: Ptr Word -> IO Word
foreign import ccall unsafe "atomic-bitops-gcc.h lock_release_word"
    lock_release_word :: Ptr Word -> IO ()

-}
