{-# Language MagicHash, UnboxedTuples, CPP  #-}

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
------------------------------------------------------------------------

import Data.Atomics (casArrayElem)

------------------------------------------------------------------------
{-# NOINLINE mynum #-}
mynum :: Int
mynum = 33

main = do 
 putStrLn "Perform a CAS within a MutableArray#"
 arr <- newArray 5 mynum

 res  <- casArrayElem arr 3 mynum 44
 res2 <- casArrayElem arr 3 mynum 44
-- res  <- stToIO$ casArrayST arr 3 mynum 44
-- res2 <- stToIO$ casArrayST arr 3 mynum 44 
 
 putStrLn$ "  1st try should succeed: "++show res
 putStrLn$ "2nd should fail: "++show res2

 putStrLn "Printing array:"
 forM_ [0..4] $ \ i -> do
   x <- readArray arr i 
   putStr ("  "++show x)
 putStrLn ""
 putStrLn "Done."
  
