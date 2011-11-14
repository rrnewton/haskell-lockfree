{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

-- This is an attempt to imitate a CAS using normal Haskell/GHC operations.
-- Useful for debugging.
-- 
-- Ryan Newton

module Data.CAS.Fake ( CASRef, casIORef, ptrEq, atomicModifyIORefCAS_ )
 where 

import Data.IORef
import Data.CAS.Class
import Debug.Trace
import System.Mem.StableName

--------------------------------------------------------------------------------

newtype CASRef a = CR { unCR :: IORef a }

instance CASable CASRef a where 
  newCASable x = newIORef x >>= (return . CR)
  readCASable  = readIORef  . unCR
  writeCASable = writeIORef . unCR
  cas          = casIORef   . unCR

--------------------------------------------------------------------------------

{-# NOINLINE casIORef #-}
-- TEMP -- A non-CAS based version.  Alas, this has UNDEFINED BEHAVIOR
-- (see ptrEq).
-- 
--  casIORef :: Eq a => IORef a -> a -> a -> IO (Bool,a)
casIORef :: IORef a -> a -> a -> IO (Bool,a)
-- casIORef r !old !new =   
casIORef r old new = do   
  atomicModifyIORef r $ \val -> 
{-
    trace ("    DBG: INSIDE ATOMIC MODIFY, ptr eqs found/expected: " ++ 
	   show [ptrEq val old, ptrEq val old, ptrEq val old] ++ 
	   " ptr eq self: " ++ 
	   show [ptrEq val val, ptrEq old old] ++
	   " names: " ++ show (unsafeName old, unsafeName old, unsafeName val, unsafeName val)
	  ) $
-}
    if   (ptrEq val old)
    then (new, (True, val))
    else (val, (False,val))

atomicModifyIORefCAS_ = atomicModifyIORef_

atomicModifyIORef_ ref fn = atomicModifyIORef ref (\ x -> (fn x, ()))

------------------------------------------------------------
-- IO versions of pointer equality:

-- ptrEq :: a -> a -> IO Bool
-- ptrEq !x !y = return (I# (reallyUnsafePtrEquality# x y) == 1)

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

