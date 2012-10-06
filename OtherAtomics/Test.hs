{-# Language MagicHash, UnboxedTuples  #-}

-- Expected output: 
{---------------------------------------
    Perform a CAS within a MutableArray#
      1st try should succeed: (True,33)
    2nd should fail: (False,44)
    Printing array:
      33  33  33  44  33
    Done.
-}
-------------------------------------------------------------------
import GHC.IO
import GHC.IORef
import GHC.ST
import GHC.STRef
import GHC.Prim
import GHC.Base
import Data.Primitive.Array
import Control.Monad

import Data.Atomics.Internal (casArray#)

------------------------------------------------------------------------

-- -- | Write a value to the array at the given index:
casArrayST :: MutableArray s a -> Int -> a -> a -> ST s (Bool, a)
casArrayST (MutableArray arr#) (I# i#) old new = ST$ \s1# ->
 case casArray# arr# i# old new s1# of 
   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

------------------------------------------------------------------------
{-# NOINLINE mynum #-}
mynum :: Int
mynum = 33

main = do 
 putStrLn "Perform a CAS within a MutableArray#"
 arr <- newArray 5 mynum

 res  <- stToIO$ casArrayST arr 3 mynum 44
 res2 <- stToIO$ casArrayST arr 3 mynum 44
 putStrLn$ "  1st try should succeed: "++show res
 putStrLn$ "2nd should fail: "++show res2

 putStrLn "Printing array:"
 forM_ [0..4] $ \ i -> do
   x <- readArray arr i 
   putStr ("  "++show x)
 putStrLn ""
 putStrLn "Done."
