module Counter (tests) where

-- This was formerly CounterCommon and #include-ed to test the different
-- counter implementations which have been removed. 
-- TODO clean up remnants of that approach.
import qualified Data.Atomics.Counter as C

import Control.Monad
import GHC.Conc
import System.CPUTime
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertBool)
import Text.Printf
import Data.IORef  

import CommonTesting (numElems, forkJoin, timeit, nTimes)

name = "Unboxed"

default_seq_tries  = 10 * numElems
-- Things are MUCH slower with contention:
default_conc_tries = numElems

--------------------------------------------------------------------------------
-- Test the basics

case_basic1 = do 
  r <- C.newCounter 0
  ret <- C.incrCounter 10 r
  assertEqual "incrCounter returns the NEW value" 10 ret

case_basic2 = do 
  r <- C.newCounter 0
  t <- C.readCounterForCAS r
  (True,newt) <- C.casCounter r t 10
  assertEqual "casCounter returns new val/ticket on success" 10 (C.peekCTicket newt)

case_basic3 = do 
  r <- C.newCounter 0
  t <- C.readCounterForCAS r
  _ <- C.incrCounter 1 r
  (False,oldt) <- C.casCounter r t 10
  assertEqual "casCounter returns read val on failure" 1 (C.peekCTicket oldt)

case_basic4 = do 
  let tries = numElems `quot` 100
  r <- C.newCounter 0
  nTimes tries $ do
    t <- C.readCounterForCAS r
    (True,_) <- C.casCounter r t (C.peekCTicket t + 1)
    return ()
  cnt <- C.readCounter r
  assertEqual "Every CAS should succeed on one thread" tries cnt 

--------------------------------------------------------------------------------
-- Repeated increments

incrloop tries = do r <- C.newCounter 0; nTimes tries $ void$ C.incrCounter 1 r
                    C.readCounter r
case_incrloop = do 
   cnt <- incrloop default_seq_tries
   assertEqual "incrloop sum" default_seq_tries cnt

-- | Here we do a loop to test the unboxing of results from incrCounter:
--   As of now [2013.07.19], it is successfully unboxing the results 
--   for Data.Atomics.Counter.Unboxed.
incrloop4B tries = do
  putStrLn " [incrloop4B] A test where we use the result of each incr."
  r <- C.newCounter 1
  loop r tries 1
 where
   loop :: C.AtomicCounter -> Int -> Int -> IO ()
   loop r 0 _ = do v <- C.readCounter r
                   putStrLn$"Final value: "++show v
                   return ()
   loop r tries last = do
     n <- C.incrCounter last r
     if n == 2
       then loop r (tries-1) 2
       else loop r (tries-1) 1

-- | Here we let the counter overflow, which seems to be causing problems.
overflowTest tries = do
  putStrLn " [incrloop4B] A test where we use the result of each incr."
  r <- C.newCounter 1
  loop r tries 1
 where
   loop :: C.AtomicCounter -> Int -> Int -> IO ()
   loop r 0 _ = do v <- C.readCounter r
                   putStrLn$"Final value: "++show v
                   return ()
   loop r tries last = do
     putStrLn$ " [incrloop4B] Looping with tries left "++show tries 
     n <- C.incrCounter last r
     -- This is HANGING afer passing 2,147,483,648.  (using Unboxed)
     -- Is there some defect wrt overflow?
     putStrLn$ " [incrloop4B] Done incr, received "++show n
     loop r (tries-1) n

--------------------------------------------------------------------------------
-- Parallel repeated increments


{-# INLINE parIncrloop #-} 
parIncrloop new incr iters = do
  numcap <- getNumCapabilities
  let (each,left) = iters `quotRem` numcap
  putStrLn$ "Concurrently incrementing counter from all "++show numcap++" threads, incrs per thread: "++show each
  r <- new 0
  forkJoin numcap $ \ ix -> do
    let mine = if ix==0 then each+left else each
    nTimes mine $ void $ incr 1 r
  C.readCounter r

case_parincrloop = do 
  cnt <- parIncrloop C.newCounter C.incrCounter default_conc_tries
  assertEqual "incrloop sum" default_conc_tries cnt

-- | Use CAS instead of the real incr so we can compare the overhead.
case_parincrloop_wCAS = do 
  cnt <- parIncrloop C.newCounter fakeIncr default_conc_tries
  assertEqual "incrloop sum" default_conc_tries cnt
 where
  fakeIncr delt r = do tick <- C.readCounterForCAS r
                       loop r delt tick
  loop r delt tick = do x <- C.casCounter r tick (C.peekCTicket tick + delt)
                        case x of 
                          (True, newtick) -> return (C.peekCTicket newtick)
                          (False,newtick) -> loop r delt newtick
                   

--------------------------------------------------------------------------------

tests = 
 [
   testCase (name++"basic1_incrCounter") $ case_basic1
 , testCase (name++"basic2_casCounter") $ case_basic2
 , testCase (name++"basic3_casCounter") $ case_basic3
 , testCase (name++"basic4_casCounter") $ case_basic4
   ----------------------------------------
 , testCase (name++"_single_thread_repeat_incr") $ timeit case_incrloop
 , testCase (name++"_incr_with_result_feedback") $ timeit (incrloop4B default_seq_tries)
   ----------------------------------------

   -- Parallel versions:
 , testCase (name++"_concurrent_repeat_incr") $ void$ timeit case_parincrloop
 , testCase (name++"_concurrent_repeat_incrCAS") $ void$ timeit case_parincrloop_wCAS
 ]
