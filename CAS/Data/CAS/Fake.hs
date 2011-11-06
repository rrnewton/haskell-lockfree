{-# LANGUAGE BangPatterns, MagicHash #-}

-- This is an attempt to imitate a CAS using normal Haskell/GHC operations.
-- Useful for debugging.
-- 
-- Ryan Newton

module Data.CAS.Fake ( casIORef, ptrEq )
 where 

import Data.IORef
import System.Mem.StableName
import GHC.IO (unsafePerformIO)

import GHC.Exts (Int(I#))
import GHC.Prim (reallyUnsafePtrEquality#)

ptrEq :: a -> a -> Bool
ptrEq x y = I# (reallyUnsafePtrEquality# x y) == 1

-- TEMP -- A non-CAS based version.  Alas, this has UNDEFINED BEHAVIOR
-- (see ptrEq).
-- 
--  casIORef :: Eq a => IORef a -> a -> a -> IO (Bool,a)
casIORef :: IORef a -> a -> a -> IO (Bool,a)
casIORef r !old !new =   
  atomicModifyIORef r $ \val -> 
--    if val == old
    if unsafePerformIO (reallyUns val old)
    then (new, (True,old))
    else (val, (False,val))


