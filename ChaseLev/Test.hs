{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import System.Environment (getEnvironment)
import Test.HUnit as HU
import Data.Array as A
import GHC.Conc (setNumCapabilities, yield)

import Control.Monad (void)
import Data.Concurrent.Deque.Tests     
import Data.Concurrent.Deque.Class
import Data.Concurrent.Deque.Debugger  (DebugDeque)
import qualified Data.Concurrent.Deque.ChaseLev as CL

import qualified Data.Atomics.Counter as C

import RegressionTests.Issue5 (standalone_pushPop)
import qualified RegressionTests.Issue5B 

main :: IO ()
main =do 
--  setNumCapabilities 4
  stdTestHarness $ do
--    setNumCapabilities 4
    theEnv <- getEnvironment    

    let fibSize = case lookup "FIBSIZE" theEnv of
                   Just s  -> read s
                   Nothing -> 38

    let newReg = (newQ :: IO (CL.ChaseLevDeque a))
        newDeb = (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
        
        all_tests :: HU.Test
        all_tests = TestList $ 
          [ appendLabel "simplest_pushPop"  $ TestCase simplest_pushPop
          , appendLabel "standalone_pushPop"  $ TestCase $ timeit standalone_pushPop
          , appendLabel "standalone_pushPop2" $ TestCase $ timeit RegressionTests.Issue5B.standalone_pushPop
          , appendLabel "ChaseLev_DbgWrapper" $ tests_wsqueue newDeb
          , appendLabel "ChaseLev"            $ tests_wsqueue newReg 
          , TestLabel "parfib" $ TestCase $ timeit$
            print =<< test_parfib_work_stealing fibSize newReg
          ]
    return all_tests

--------------------------------------------------------------------------------
-- Individual unit and regression tests:
-------------------------------------------------------------------------------

-- <Small Reproducer for recent debug wrapper problem>
-- This fails even without profiling on.
simplest_pushPop :: IO ()
simplest_pushPop =
  triv =<< (newQ :: IO (DebugDeque CL.ChaseLevDeque a))           
 where   
   -- This is what's failing with the debug wrapper, WHY?
   triv :: PopL d => d [Char] -> IO ()
   triv q = do
     pushL q "hi" 
     x <- tryPopL q
     let y = case x of
              Just z -> z
              Nothing -> error "Even a single push/pop in isolation did not work!"
     assertEqual "test_ws_triv1" y "hi"


{-# INLINE test_parfib_work_stealing #-}
test_parfib_work_stealing :: (DequeClass d, PopL d) => Elt -> IO (d Elt) -> IO Elt
test_parfib_work_stealing origInput newqueue = do
  putStrLn$ " [parfib] Computing fib("++show origInput++")"
  numAgents <- getNumAgents
  qs <- sequence (replicate numAgents newqueue)
  let arr = A.listArray (0,numAgents - 1) qs 
  
  let parfib !myId !myQ !mySum !num
        | num <= 2  =
          do x <- tryPopL myQ
             case x of
               Nothing -> trySteal myId myQ (mySum+1)
               Just n  -> parfib myId myQ   (mySum+1) n
        | otherwise = do 
          pushL       myQ       (num-1)
          parfib myId myQ mySum (num-2)
          
      trySteal !myId !myQ !mySum =
        let loop ind
              -- After we finish one sweep... we're completely done.
              | ind == myId     = return mySum
              | ind == size arr = loop 0
              | otherwise = do
                  x <- tryPopR (arr ! ind)
                  case x of
                    Just n  -> parfib myId myQ mySum n
                    Nothing -> do yield
                                  loop (ind+1)
        in loop (myId+1)

      size a = let (st,en) = A.bounds a in en - st + 1 
  
  partial_sums <- forkJoin numAgents $ \ myId ->
    if myId == 0
    then parfib   myId (arr ! myId) 0 origInput
    else trySteal myId (arr ! myId) 0 
  
  return (sum partial_sums)

{-
NOTES ON PARFIB PERFORMANCE
===========================

Here are some old notes for a point of comparison:

[2011.03] On 4-core nehalem, 3.33ghz:
-------------------------------------

  Non-monadic version, real/user time:
  fib(40) 4 threads: 1.1s 4.4s
  fib(42) 1 threads: 9.7s  
  fib(42) 4 threads: 2.86s 11.6s  17GB allocated -- 3.39X
  
     SPARKS: 433784785 (290 converted, 280395620 pruned)

  Monad-par version:
  fib(38) non-threaded: 23.3s 23.1s
  fib(38) 1 thread :    24.7s 24.5s
  fib(38) 4 threads:     8.2s 31.3s

  fib(40) 4 threads:    20.6s 78.6s 240GB allocated


For comparison, Cilkarts Cilk++:
  fib(42) 4 threads:  3.029s 23.610s

Intel Cilk Plus:
  fib(42) 4 threads:  4.212s 16.770s

   1 thread: 17.53 -- e.g. 4.16X speedup


[2013.07.18] {Running with the ChaseLev-in-Haskell deques}
----------------------------------------------------------

Running initial timing tests with the new parfib test and a version of ChaseLev that
uses "Foreign" atomic counters.
On a 3.1 Ghz 4-core westmere desktop, running with +RTS -qa:

  fib(41) 1 thread : 10.9s    (99.7% productivity, 7.9GB alloc, 5mb copied)
  fib(41) 4 threads: 3.88s    (72% productivity, 7.9 GB alloc, 5mb copied)

  fib(42) 1 threads: 17.6s    (99.6% prod)
  fib(42) 4 threads: 6.19s    (70.5% productivity, 12.8G alloc)

  fib(43) 1 threads: 28.7s    (99.6% prod)
  fib(43) 4 threads: 10.2s    (63% productivity, 20.8 GB alloc)

-}
