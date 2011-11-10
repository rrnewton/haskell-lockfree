{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns, MagicHash,
    TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | Atomic compare and swap for IORefs and CASRefs.
module Data.CAS 
 ( casSTRef, casIORef, CASRef )
where

import Data.CAS.Class
import GHC.IO
import GHC.IORef
import GHC.Prim
import GHC.ST
import GHC.STRef

--------------------------------------------------------------------------------

newtype CASRef a = CR { unCR :: IORef a }

instance CASable CASRef a where 
  newCASable x = newIORef x >>= (return . CR)
  readCASable  = readIORef  . unCR
  writeCASable = writeIORef . unCR
  cas          = casIORef   . unCR

--------------------------------------------------------------------------------

-- | Performs a machine-level compare and swap operation on an
-- 'STRef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'STRef'.
casSTRef :: STRef s a -- ^ The 'STRef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> ST s (Bool, a) 
casSTRef (STRef var#) old new = ST $ \s1# ->
   -- The primop treats the boolean as a sort of error code.
   -- Zero means the CAS worked, one that it didn't.
   -- We flip that here:
    case casMutVar# var# old new s1# of
      (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

-- | Performs a machine-level compare and swap operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'IORef'.
casIORef :: IORef a -- ^ The 'IORef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (Bool, a) 
casIORef (IORef var) old new = stToIO (casSTRef var old new)
      

