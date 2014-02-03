{-# LANGUAGE OverlappingInstances, MultiParamTypeClasses, BangPatterns, MagicHash #-}

-- | A type class capturing mutable storage cells that support CAS
--   operations in the IO monad.

module Data.CAS.Internal.Class 
  (CASable(..), unsafeName, ptrEq) 
 where

import GHC.IO (unsafePerformIO)
import GHC.Exts (Int(I#))
import GHC.Prim (reallyUnsafePtrEquality#)
import System.Mem.StableName

-- | It would be nice to use an associated type family with this class
--   (for casref), but that would preclude overlapping instances.
class CASable casref a where 
  newCASable   :: a -> IO (casref a)
  readCASable  :: casref a -> IO a 
  writeCASable :: casref a -> a -> IO ()
  cas          :: casref a -> a -> a -> IO (Bool,a)


{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)

{-# NOINLINE ptrEq #-}
ptrEq :: a -> a -> Bool
ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1
