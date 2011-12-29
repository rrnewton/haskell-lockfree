{-# LANGUAGE CPP
  #-}

-- | Atomic compare and swap for IORefs and STRefs.
module Data.CAS 
 ( 
  -- Not currently provided by Fake.hs:
  -- casSTRef, 
   casIORef, ptrEq,
   atomicModifyIORefCAS, atomicModifyIORefCAS_,

   -- * Generic interface: for interoperation with `Fake` and `Foreign` alternative libraries.
   CASRef)
where

#if __GLASGOW_HASKELL__ <= 702  /* Fix to casMutVar introduced 2011.12.09 */
#warning "casMutVar is not included or is bugged in your GHC, falling back to Fake version."
import Data.CAS.Internal.Fake
#else
import Data.CAS.Internal.Native
#endif
