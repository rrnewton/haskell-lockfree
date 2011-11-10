{-# LANGUAGE BangPatterns, MagicHash, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- This is an attempt to imitate a CAS using normal Haskell/GHC operations.
-- Useful for debugging.
-- 
-- Ryan Newton

module Data.CAS.Fake ( CASRef, casIORef, ptrEq )
 where 

import Data.IORef
import Data.CAS.Class
import System.Mem.StableName
import GHC.IO (unsafePerformIO)

import GHC.Exts (Int(I#))
import GHC.Prim (reallyUnsafePtrEquality#)

--------------------------------------------------------------------------------

newtype CASRef a = CR { unCR :: IORef a }

instance CASable CASRef a where 
  newCASable x = newIORef x >>= (return . CR)
  readCASable  = readIORef  . unCR
  writeCASable = writeIORef . unCR
  cas          = casIORef   . unCR

--------------------------------------------------------------------------------

ptrEq :: a -> a -> Bool
ptrEq x y = I# (reallyUnsafePtrEquality# x y) == 1

-- TEMP -- A non-CAS based version.  Alas, this has UNDEFINED BEHAVIOR
-- (see ptrEq).
-- 
--  casIORef :: Eq a => IORef a -> a -> a -> IO (Bool,a)
casIORef :: IORef a -> a -> a -> IO (Bool,a)
-- casIORef r !old !new =   
casIORef r old new = do   
  atomicModifyIORef r $ \val -> 
    if (ptrEq val old)
    then (new, (True,old))
    else (val, (False,val))

{-# INLINE nameEq #-}
-- WARNING:  This has completely implementation-defined behavior. 
--   mkStableName + (==) provides no guarantee against false negatives.
--     http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-Mem-StableName.html
nameEq :: a -> a -> IO Bool
nameEq !a !b = do 
  s1 <- makeStableName a
  s2 <- makeStableName b
--  printf "    comparing ptrs with stablenames %d %d...\n" (hashStableName s1) (hashStableName s2)
  return (s1 == s2)

