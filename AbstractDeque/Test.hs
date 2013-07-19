{-# LANGUAGE CPP, ScopedTypeVariables, NamedFieldPuns, BangPatterns #-}
#if __GLASGOW_HASKELL >= 700
{-# OPTIONS_GHC -with-rtsopts=-K32M #-}
#endif

import Data.Concurrent.Deque.Class
-- import Data.Concurrent.Deque.Class.Reference (newQueue)
-- import Data.Concurrent.MegaDeque 
import Data.Int
import Data.Array as A
import Control.Concurrent (yield)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit     (hUnitTestToTests)
import Test.HUnit (assert, assertEqual, Test(TestCase, TestList, TestLabel)) 
import qualified Data.Concurrent.Deque.Tests as T
import qualified Data.Concurrent.Deque.Reference as R
import qualified Data.Concurrent.Deque.Class as C
import Data.Concurrent.Deque.Debugger (DebugDeque)
import System.Environment (withArgs)

-- Import the instances:
import Data.Concurrent.Deque.Reference.DequeInstance ()

test_1 :: Test
test_1 = TestCase $ assert $ 
  do q <- R.newQ -- Select a specific implementation.
     pushR q 3
     Just x <- tryPopR q
     assertEqual "test_1 result" x (3::Integer)

test_2 :: Test
test_2 = TestCase $ assert $ 
  do 
     -- Here's an example of type-based restriction of the queue implementation:
     q <- newQ :: IO (Deque NT T D S Bound Safe Int)
     pushL q 33
--     pushR q 33  -- This would cause a type error because the Right end is not Double-capable.
     Just x <- tryPopR q
     assertEqual "test_2 result" x 33

test_parfib_work_stealing_specialized :: T.Elt -> IO T.Elt
test_parfib_work_stealing_specialized origInput = do
  putStrLn$ " [parfib] Computing fib("++show origInput++")"
  numAgents <- T.getNumAgents
  qs <- sequence (replicate numAgents R.newQ)
  let arr = A.listArray (0,numAgents - 1) qs 
  
  let parfib !myId !myQ !mySum !num
        | num <= 2  =
          do x <- R.tryPopL myQ
             case x of
               Nothing -> trySteal myId myQ (mySum+1)
               Just n  -> parfib myId myQ   (mySum+1) n
        | otherwise = do 
          R.pushL    myQ       (num-1)
          parfib myId myQ mySum (num-2)
          
      trySteal !myId !myQ !mySum =
        let loop ind
              -- After we finish one sweep... we're completely done.
              | ind == myId     = return mySum
              | ind == size arr = loop 0
              | otherwise = do
                  x <- R.tryPopR (arr ! ind)
                  case x of
                    Just n  -> parfib myId myQ mySum n
                    Nothing -> do yield
                                  loop (ind+1)
        in loop (myId+1)

      size a = let (st,en) = A.bounds a in en - st + 1 
  
  partial_sums <- T.forkJoin numAgents $ \ myId ->
    if myId == 0
    then parfib   myId (arr ! myId) 0 origInput
    else trySteal myId (arr ! myId) 0 
  
  return (sum partial_sums)

main :: IO ()
#if __GLASGOW_HASKELL__ >= 700
main = T.stdTestHarness $ return all_tests
 where 
 all_tests :: Test
 all_tests = 
   T.appendLabels "Reference_Deque" $ 
     [ T.appendLabel "test_1" test_1
     , T.appendLabel "test_2" test_2 
     , T.appendLabel "direct"$ T.tests_all R.newQ
     -- Test going through the class interface as well:  
     , T.appendLabel "thru_class"$ T.tests_all (C.newQ :: IO (R.SimpleDeque a))
     , T.appendLabel "with_debug"$ T.tests_all (C.newQ :: IO (DebugDeque R.SimpleDeque a))
       
     , TestLabel "parfib_specialized" $ TestCase $ T.timeit$
       print =<< test_parfib_work_stealing_specialized T.fibSize
     ]

-- main = do 
--   putStrLn "[ Test executable: test reference deque implementation... ]"
--   withArgs ["-j1","--jxml=test-results.xml"] $   
--     defaultMain$ hUnitTestToTests$
 
#else
main = putStrLn "WARNING: Tests disabled for GHC < 7"
#endif
