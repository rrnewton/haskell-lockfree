{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import System.Environment (getEnvironment)
import Test.HUnit as HU
import Data.Int
import Data.Array as A
import GHC.Conc (setNumCapabilities, yield)

import Control.Monad (void)
import Data.Concurrent.Deque.Tests     
import Data.Concurrent.Deque.Class
import Data.Concurrent.Deque.Debugger  (DebugDeque)
import qualified Data.Concurrent.Deque.ChaseLev as CL
import qualified Data.Concurrent.Deque.ChaseLevUnboxed as CU

import qualified Data.Atomics.Counter as C

import RegressionTests.Issue5 (standalone_pushPop)
import qualified RegressionTests.Issue5B 

main :: IO ()
main =do 
--  setNumCapabilities 4
  stdTestHarness $ do
--    setNumCapabilities 4
    theEnv <- getEnvironment    

    let newReg = (newQ :: IO (CL.ChaseLevDeque a))
        newDeb = (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
        
        all_tests :: HU.Test
        all_tests = TestList $ 
          [ appendLabel "simplest_pushPop"  $ TestCase simplest_pushPop
          , appendLabel "standalone_pushPop"  $ TestCase $ timeit standalone_pushPop
          , appendLabel "standalone_pushPop2" $ TestCase $ timeit RegressionTests.Issue5B.standalone_pushPop
--          , appendLabel "ChaseLev_DbgWrapper" $ tests_wsqueue newDeb
          , appendLabel "ChaseLev"            $ tests_wsqueue newReg
            -- Even with inlining this isn't working:
          -- , TestLabel "parfib_generic" $ TestCase $ timeit$
          --   print =<< test_parfib_work_stealing fibSize newReg
          , TestLabel "parfib_specialized_boxed" $ TestCase $ timeit$
            print =<< test_parfib_work_stealing_specialized fibSize
          , TestLabel "parfib_specialized_unboxed" $ TestCase $ timeit$
            print =<< test_parfib_work_stealing_specialized_unboxed fibSize             
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


test_parfib_work_stealing_specialized :: Elt -> IO Elt
test_parfib_work_stealing_specialized origInput = do
  putStrLn$ " [parfib] Computing fib("++show origInput++")"
  numAgents <- getNumAgents
  qs <- sequence (replicate numAgents CL.newQ)
  let arr = A.listArray (0,numAgents - 1) qs 
  
  let parfib !myId !myQ !mySum !num
        | num <= 2  =
          do x <- CL.tryPopL myQ
             case x of
               Nothing -> trySteal myId myQ (mySum+1)
               Just n  -> parfib myId myQ   (mySum+1) n
        | otherwise = do 
          CL.pushL    myQ       (num-1)
          parfib myId myQ mySum (num-2)
          
      trySteal !myId !myQ !mySum =
        let loop ind
              -- After we finish one sweep... we're completely done.
              | ind == myId     = return mySum
              | ind == size arr = loop 0
              | otherwise = do
                  x <- CL.tryPopR (arr ! ind)
                  case x of
                    Just n  -> parfib myId myQ mySum n
                    Nothing -> do -- yield
                                  -- threaDelay 1000
                                  loop (ind+1)
        in loop (myId+1)

      size a = let (st,en) = A.bounds a in en - st + 1 
  
  partial_sums <- forkJoin numAgents $ \ myId ->
    if myId == 0
    then parfib   myId (arr ! myId) 0 origInput
    else trySteal myId (arr ! myId) 0 
  
  return (sum partial_sums)

-- DUPLICATED CODE:
test_parfib_work_stealing_specialized_unboxed :: Elt -> IO Elt
test_parfib_work_stealing_specialized_unboxed origInput = do
  putStrLn$ " [parfib] Computing fib("++show origInput++")"
  numAgents <- getNumAgents
  qs <- sequence (replicate numAgents CU.newQ)
  let arr = A.listArray (0,numAgents - 1) qs 
  
  let parfib !myId !myQ !mySum !num
        | num <= 2  =
          do x <- CU.tryPopL myQ
             case x of
               Nothing -> trySteal myId myQ (mySum+1)
               Just n  -> parfib myId myQ   (mySum+1) n
        | otherwise = do 
          CU.pushL    myQ       (num-1)
          parfib myId myQ mySum (num-2)
          
      trySteal !myId !myQ !mySum =
        let loop ind
              -- After we finish one sweep... we're completely done.
              | ind == myId     = return mySum
              | ind == size arr = loop 0
              | otherwise = do
                  x <- CU.tryPopR (arr ! ind)
                  case x of
                    Just n  -> parfib myId myQ mySum n
                    Nothing -> do -- yield
                                  -- threaDelay 1000
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

Experimenting with a few optimizations...
Hmm, when I was running parfib_specialized on 4 threads I noticed it using 300% cpu if I used -qa.
Oops, since this isn't using forkOn it should NOT be using -qa.

Aha!  With the specialized version we can get it down to this:

  fib(42) 1 threads: 13.6   
  fib(42) 4 threads: 5.46    (69% prod, 8.5G alloc)

WEIRD! Switching back to Counter.Reference actually gets a SPEEDUP at this point,
bringing the above down to as low as 4.48s, in spite of a whopping 23GB allocation
and 50% productivity.  While Counter.Foreign dominates in the highest contention
scenarious, the FFI tax must be hurting in this lower-contention example.
Perhaps if we exposed primops to perf atomic ops directly on byte arrays...

Hmm... I think the variance is higher in this mode.  Let's try Counter.IORef.
That one is actually SLOWER than atomicModifyIORef.  How can that be?

With a bit more optimization/INLINING I can get fib(42) down to 4.19s (Reference).
Still high variance and 19.2G allocation though...  I get as low as 5.17s for Foreign
and 4.2G alloc.  Actually, now IORef is doing almost as well as Reference, and it is
better under high contention.  So making that the default for now.

The Data.Seq based reference implementation in abstract-deque takes 6.6 seconds for
fib(42).  

[2013.07.19] {A few more updates}
------------------------------------

Ok, with the final "Data.Atomics.Counter.Unboxed" implementation, the fib(42) test
now takes as little as 3.12 seconds.  That is competitive with Cilk and Haskell
sparks, however, its not a fair comparison, because we are literally pushing numbers
through the queue, not continuations/thunks.  Although its not as far off as it might
be because currently our ChaseLev deque works with lifted/thunked values anyway, so
our "numbers" are not hugely dissimilar from continuations.

(Ideally we could use closed type families to transparently optimize for the unboxed case.)

If I COPY PASTE the ChaseLev implementation to produce an Unboxed version... it runs
a little quicker, 3.0 seconds.  And, nicely, it eliminates almost all allocation,
from 4GB to a few MB.

I still need to do the "local topBound" optimization.

Testing on Hive
---------------

Unboxed version on a bigger, 32 core machine, lower clock speed (2.13ghz):

    fib(42) 1 threads:  21s
    fib(42) 2 threads:  10.1s
    fib(42) 4 threads:  5.2s (100%prod)
    fib(42) 8 threads:  2.7s - 3.2s (100%prod)  (whoa, some runs up to 7.87s! very random)
    fib(42) 16 threads: 1.28s
    fib(42) 24 threads: 1.85s
    fib(42) 32 threads: 4.8s (high variance)

As usual, without pinning, which cores get hit is kind of random (same socket,
different socket).  I can confirm that just by watching htop.

I wonder why scaling is falling off given that it's 100% productivity even at 32
cores.  The BOXED version actually isn't much worse though at 32 cores:

    (boxed) REALTIME 1.470864s 3.526944s 4.890355s

The "Counter.Unboxed" implementation does help though... if we do
ChaselevUnboxed/Counter.Foreign, then we see vastly worse times:

    (hive/foreignCounter) fib(42) 32 threads: 14.4s

And double the time on the 4-core as well:

    (travertine/foreignCounter) fib(42) 4 threads: 6.6s

And needless to say the reference implementation (IORef Data.Seq) doesn't scale.
Let's call this the "legacy" implementation:

    (travertine/legacy) fib(42) 4 threads: 6.6s

    (hive) fib(42) 1 threads:  41.8s  (95% prod)
    (hive) fib(42) 2 threads:  25.2s  (66% prod)
    (hive) fib(42) 4 threads:  14.6s  (27% prod, 135GB alloc)
    (hive) fib(42) 8 threads:  17.1s  (26% prod)
    (hive) fib(42) 16 threads: 16.3s  (13% prod)
    (hive) fib(42) 24 threads: 21.2s  (30% prod)
    (hive) fib(42) 32 threads: 29.3s  (33% prod)

-}
