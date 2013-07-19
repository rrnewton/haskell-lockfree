{-# LANGUAGE BangPatterns #-}

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
import qualified Data.Atomics.Counter.Unboxed   as C4

import CommonTesting (numElems, forkJoin, timeit)

--------------------------------------------------------------------------------

-- {-# INLINE nTimes #-}
-- | To make sure we get a simple loop...
nTimes :: Int -> IO () -> IO ()
-- nTimes :: Int -> IO a -> IO ()
-- Note: starting out I would get 163Mb allocation for 10M sequential incrs (on unboxed).
-- The problem was that the "Int" result from each incr was being allocated.
-- Weird thing is that inlining nTimes reduces the allocation to 323Mb.
-- But forcing it to take an (IO ()) gets rid of the allocation.
-- Egad, wait, no, I have to NOT inline nTimes to get rid of the allocation!?!?
-- Otherwise I'm still stuck with at least 163Mb of allocation.
-- In fact... the allocation is still there even if we use incrCounter_ !!
-- If we leave nTimes uninlined, we can get down to 3Mb allocation with either incrCounter or incrCounter_.
-------------------------
-- UPDATE:
-- As per http://www.haskell.org/pipermail/glasgow-haskell-users/2011-June/020472.html
--
--  INLINE should not affect recursive functions.  But here it seems to have a
--  deleterious effect!
nTimes 0 !c = return ()
nTimes !n !c = c >> nTimes (n-1) c

-- {-# INLINE vd #-}
-- vd = void
-- vd x = x

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

-- This is not allocating per-iteration currently, which is rather amazing given what
-- casIORef returns.
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

incrloop1 tries = do r <- C1.newCounter 0; nTimes tries $ void$ C1.incrCounter 1 r
incrloop2 tries = do r <- C2.newCounter 0; nTimes tries $ void$ C2.incrCounter 1 r
incrloop3 tries = do r <- C3.newCounter 0; nTimes tries $ void$ C3.incrCounter 1 r
incrloop4 tries = do r <- C4.newCounter 0; nTimes tries $ C4.incrCounter_ 1 r

-- | Here we do a loop to test the unboxing of results from incrCounter:
--   As of now [2013.07.19], it is successfully unboxing the results.
incrloop4B tries = do
  putStrLn " [incrloop4B] A test where we use the result of each incr."
  r <- C4.newCounter 1
  loop r tries 1
 where
   loop :: C4.AtomicCounter -> Int -> Int -> IO ()
   loop r 0 _ = do v <- C4.readCounter r
                   putStrLn$"Final value: "++show v
                   return ()
   loop r tries last = do
     n <- C4.incrCounter last r
     if n == 2
       then loop r (tries-1) 2
       else loop r (tries-1) 1

-- | Here we let the counter overflow, which seems to be causing problems.
overflowTest tries = do
  putStrLn " [incrloop4B] A test where we use the result of each incr."
  r <- C4.newCounter 1
  loop r tries 1
 where
   loop :: C4.AtomicCounter -> Int -> Int -> IO ()
   loop r 0 _ = do v <- C4.readCounter r
                   putStrLn$"Final value: "++show v
                   return ()
   loop r tries last = do
     putStrLn$ " [incrloop4B] Looping with tries left "++show tries 
     n <- C4.incrCounter last r
     -- This is HANGING afer passing 2,147,483,648.
     -- Is there some defect wrt overflow?
     putStrLn$ " [incrloop4B] Done incr, received "++show n
     loop r (tries-1) n



{-# INLINE parIncrloop #-} 
parIncrloop new incr iters = do
  numcap <- getNumCapabilities
  let each = iters `quot` numcap
  putStrLn$ "Concurrently incrementing counter from all "++show numcap++" threads, incrs per thread: "++show each
  r <- new 0
  forkJoin numcap $ \ _ ->    
    nTimes each $ void $ incr 1 r
  return r

parIncrloop1 = parIncrloop C1.newCounter C1.incrCounter
parIncrloop2 = parIncrloop C2.newCounter C2.incrCounter
parIncrloop3 = parIncrloop C3.newCounter C3.incrCounter
parIncrloop4 = parIncrloop C4.newCounter C4.incrCounter

--------------------------------------------------------------------------------

default_seq_tries  = 10 * numElems
-- Things are MUCH slower with contention:
default_conc_tries = numElems

counterTests = 
 [
   ----------------------------------------
   testCase "RAW_single_thread_repeat_flip" $ do 
     putStrLn "Timing readIORef/writeIORef on one thread"
     timeit (normal default_seq_tries)   
 , testCase "CAS_single_thread_repeat_flip" $ do 
     putStrLn "Timing CAS boolean flips on one thread without retries"
     fin <- timeit (casBased default_seq_tries)
     putStrLn$"Final value: "++show fin
   ----------------------------------------
 , testCase "RAW_single_thread_repeat_incr" $ do 
     putStrLn "Timing readIORef/writeIORef on one thread"
     fin <- timeit (normalIncr default_seq_tries)
     putStrLn$"Final value: "++show fin      
 , testCase "CAS_single_thread_repeat_incr" $ do 
     putStrLn "Timing CAS increments on one thread without retries"
     fin <- timeit (casBasedIncr default_seq_tries)
     putStrLn$"Final value: "++show fin 
   ----------------------------------------
 , testCase "CounterReference_single_thread_repeat_incr" $ timeit (incrloop1 default_seq_tries)
 , testCase "CounterIORef_single_thread_repeat_incr"     $ timeit (incrloop2 default_seq_tries)   
 , testCase "CounterForeign_single_thread_repeat_incr"   $ timeit (incrloop3 default_seq_tries)
 , testCase "CounterUnboxed_single_thread_repeat_incr"   $ timeit (incrloop4 default_seq_tries)
 , testCase "CounterUnboxed_incr_with_result_feedback"   $ timeit (incrloop4B default_seq_tries)
   ----------------------------------------

   -- Parallel versions:
 , testCase "CounterReference_concurrent_repeat_incr" $ void$ timeit (parIncrloop1 default_conc_tries)
 , testCase "CounterIORef_concurrent_repeat_incr"     $ void$ timeit (parIncrloop2 default_conc_tries)
 , testCase "CounterForeign_concurrent_repeat_incr"   $ void$ timeit (parIncrloop3 default_conc_tries)
 , testCase "CounterUnboxed_concurrent_repeat_incr"   $ void$ timeit (parIncrloop4 default_conc_tries)
 ]
