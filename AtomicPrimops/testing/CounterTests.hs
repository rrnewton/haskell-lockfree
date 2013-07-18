
-- | Test the counter implementation alternatives.

module CounterTests where

import Control.Monad
import GHC.Conc
import System.CPUTime
import Test.Framework.Providers.HUnit (testCase)
import Text.Printf
import Data.IORef  

import Data.Atomics
import qualified Data.Atomics.Counter.Reference as C1 
import qualified Data.Atomics.Counter.IORef     as C2
import qualified Data.Atomics.Counter.Foreign   as C3

import CommonTesting (numElems, forkJoin)

--------------------------------------------------------------------------------

-- | To make sure we get a simple loop...
nTimes :: Int -> IO a -> IO ()
nTimes 0 c = return ()
nTimes n c = c >> nTimes (n-1) c

cputime :: IO t -> IO t
cputime a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "SELFTIMED: %0.3f sec\n" (diff :: Double)
    return v

normal tries = do
  r <- newIORef True
  nTimes tries $ do
    x <- readIORef r
    writeIORef r $! not x
    
casBased tries = do    
  r <- newIORef True
  nTimes tries $ do
    t <- readForCAS r
    casIORef r t (not $ peekTicket t)
    return ()
  readIORef r

normalIncr tries = do
  r <- newIORef 0
  nTimes tries $ do
    x <- readIORef r
    writeIORef r $! 1 + x
  readIORef r

casBasedIncr tries = do    
  r <- newIORef 0
  nTimes tries $ do
    t <- readForCAS r
    casIORef r t $! (1 + peekTicket t)
    return ()
  readIORef r

incrloop1 tries = do
  r <- C1.newCounter 0
  nTimes tries $ C1.incrCounter 1 r

incrloop2 tries = do
  r <- C2.newCounter 0
  nTimes tries $ C2.incrCounter 1 r

incrloop3 tries = do
  r <- C3.newCounter 0
  nTimes tries $ C3.incrCounter 1 r

{-# INLINE parIncrloop #-} 
parIncrloop new incr iters = do
  numcap <- getNumCapabilities
  let each = iters `quot` numcap
  putStrLn$ "Concurrently incrementing counter from all "++show numcap++" threads, incrs per thread: "++show each
  r <- new 0
  forkJoin numcap $ \ _ ->    
    nTimes each $ incr 1 r
  return r

parIncrloop1 = parIncrloop C1.newCounter C1.incrCounter
parIncrloop2 = parIncrloop C2.newCounter C2.incrCounter
parIncrloop3 = parIncrloop C3.newCounter C3.incrCounter

--------------------------------------------------------------------------------

default_seq_tries  = 10 * numElems
-- Things are MUCH slower with contention:
default_conc_tries = numElems

counterTests = 
 [
   ----------------------------------------
   testCase "RAW_single_thread_repeat_flip" $ do 
     putStrLn "Timing readIORef/writeIORef on one thread"
     cputime (normal default_seq_tries)   
 , testCase "CAS_single_thread_repeat_flip" $ do 
     putStrLn "Timing CAS boolean flips on one thread without retries"
     fin <- cputime (casBased default_seq_tries)
     putStrLn$"Final value: "++show fin
   ----------------------------------------
 , testCase "RAW_single_thread_repeat_incr" $ do 
     putStrLn "Timing readIORef/writeIORef on one thread"
     fin <- cputime (normalIncr default_seq_tries)
     putStrLn$"Final value: "++show fin      
 , testCase "CAS_single_thread_repeat_incr" $ do 
     putStrLn "Timing CAS increments on one thread without retries"
     fin <- cputime (casBasedIncr default_seq_tries)
     putStrLn$"Final value: "++show fin 
   ----------------------------------------
 , testCase "CounterReference_single_thread_repeat_incr" $ cputime (incrloop1 default_seq_tries)
 , testCase "CounterIORef_single_thread_repeat_incr"     $ cputime (incrloop2 default_seq_tries)   
 , testCase "CounterForeign_single_thread_repeat_incr"   $ cputime (incrloop3 default_seq_tries)   
   ----------------------------------------

   -- Parallel versions:
 , testCase "CounterReference_concurrent_repeat_incr" $ void$ cputime (parIncrloop1 default_conc_tries)
 , testCase "CounterIORef_concurrent_repeat_incr"     $ void$ cputime (parIncrloop2 default_conc_tries)
 , testCase "CounterForeign_concurrent_repeat_incr"   $ void$ cputime (parIncrloop3 default_conc_tries)
 ]
