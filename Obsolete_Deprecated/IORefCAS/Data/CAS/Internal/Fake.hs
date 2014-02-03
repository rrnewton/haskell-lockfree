{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}
-- Author: Ryan Newton

-- | This is an attempt to imitate a CAS using normal Haskell/GHC operations.
-- Useful for debugging.
-- 

module Data.CAS.Internal.Fake 
 ( CASRef, casIORef, ptrEq, 
   atomicModifyIORefCAS, atomicModifyIORefCAS_ 
 )
 where 

import Data.IORef
import Data.CAS.Internal.Class
import Debug.Trace
import System.Mem.StableName

--------------------------------------------------------------------------------

-- | The type of references supporting CAS.
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

atomicModifyIORefCAS  = atomicModifyIORef
atomicModifyIORefCAS_ = atomicModifyIORef_

atomicModifyIORef_ ref fn = atomicModifyIORef ref (\ x -> (fn x, ()))

