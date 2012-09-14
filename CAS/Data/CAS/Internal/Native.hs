{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns,
    TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Data.CAS.Internal.Native
 ( CASRef, casIORef, ptrEq, 
   atomicModifyIORefCAS, atomicModifyIORefCAS_ 
 )
 where 

import Data.CAS.Internal.Class
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
-- 
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.
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
-- 
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.
casIORef :: IORef a -- ^ The 'IORef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (Bool, a) 
casIORef (IORef var) old new = stToIO (casSTRef var old new)

-- | A drop-in replacement for `atomicModifyIORefCAS` that
--   optimistically attempts to compute the new value and CAS it into
--   place without introducing new thunks or locking anything.  Note
--   that this is more STRICT than its standard counterpart and will only
--   place evaluated (WHNF) values in the IORef.
atomicModifyIORefCAS :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefCAS ref fn = do
-- TODO: Should handle contention in a better way.
   init <- readIORef ref
   loop init effort
  where 
   effort = 30 :: Int -- TODO: Tune this.
   loop old 0     = atomicModifyIORef ref fn
   loop old tries = do 
     (new,result) <- evaluate (fn old)
     (b,val) <- casIORef ref old new
     if b 
      then return result
      else loop val (tries-1)

-- | A simpler version that modifies the state but does not return anything.
atomicModifyIORefCAS_ :: IORef t -> (t -> t) -> IO ()
-- atomicModifyIORefCAS_ ref fn = atomicModifyIORefCAS ref (\ x -> (fn x, ()))
-- Can't inline a function with a loop so we duplicate this:
-- <duplicated code>
atomicModifyIORefCAS_ ref fn = do
   init <- readIORef ref
   loop init effort
  where 
   effort = 30 :: Int -- TODO: Tune this.
   loop old 0     = atomicModifyIORef_ ref fn
   loop old tries = do 
     new <- evaluate (fn old)
     (b,val) <- casIORef ref old new
     if b 
      then return ()
      else loop val (tries-1)
   atomicModifyIORef_ ref fn = atomicModifyIORef ref (\ x -> (fn x, ()))
-- </duplicated code>
