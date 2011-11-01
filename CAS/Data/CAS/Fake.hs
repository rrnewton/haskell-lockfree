{-# LANGUAGE BangPatterns #-}

-- This is an attempt to imitate a CAS using normal Haskell/GHC operations.
-- Useful for debugging.
-- 
-- Ryan Newton

module Data.CAS.Fake ( casIORef, ptrEq )
 where 

import Data.IORef
import System.Mem.StableName
import GHC.IO (unsafePerformIO)

-- TEMP -- A non-CAS based version.  Alas, this has UNDEFINED BEHAVIOR
-- (see ptrEq).
-- 
--  casIORef :: Eq a => IORef a -> a -> a -> IO (Bool,a)
casIORef :: IORef a -> a -> a -> IO (Bool,a)
casIORef r !old !new =   
  atomicModifyIORef r $ \val -> 
--    if val == old
    if unsafePerformIO (ptrEq val old)
    then (new, (True,old))
    else (val, (False,val))


-- TEMP:
-- instance Eq a => Eq (Pair a) where 
--   Null == Null         = True
--   Cons a b == Cons c d = 
--     if a == c then unsafePerformIO $ do
--       s1 <- makeStableName b
--       s2 <- makeStableName d
--       return (s1 == s2)
--     else False
--   _ == _               = False


------------------------------------------------------------

{-# INLINE ptrEq #-}
-- WARNING:  This has completely implementation-defined behavior. 
--   mkStableName + (==) provides no guarantee against false negatives.
--     http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-Mem-StableName.html
ptrEq :: a -> a -> IO Bool
ptrEq !a !b = do 
  s1 <- makeStableName a
  s2 <- makeStableName b
--  printf "    comparing ptrs with stablenames %d %d...\n" (hashStableName s1) (hashStableName s2)
  return (s1 == s2)
