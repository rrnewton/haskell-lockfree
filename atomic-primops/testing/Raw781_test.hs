{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- Test the primops in GHC 7.8.1 directly.

module Main where

import GHC.Prim
import GHC.IORef
import GHC.STRef
import GHC.ST
import GHC.IO

{-# NOINLINE str #-}
str :: String
str = "hello"

t :: MutVar# d a -> a -> a -> State# d -> (# State# d, Int#, a #)
t = casMutVar#

(===) :: Int# -> Int# -> Bool
(===) x y = case x ==# y of { 0# -> False; _ -> True }

----------------------------------------

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
      (# s2#, x#, res #) -> (# s2#, (x# === 0#, res) #)

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

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  r <- newIORef str
  pr <- casIORef r str "new str"
  print pr
