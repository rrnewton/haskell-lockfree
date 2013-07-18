{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import System.Environment (getEnvironment)
import Test.HUnit as HU
import Data.Array as A
import GHC.Conc (setNumCapabilities)

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
    let wrapper = case lookup "NOWRAPPER" theEnv of
                   Just _  -> False
                   Nothing -> True
    let plain = case lookup "ONLYWRAPPER" theEnv of
                  Just _  -> False
                  Nothing -> True  
    let all_tests :: HU.Test
        all_tests = TestList $ 
          [ appendLabel "simplest_pushPop"  $ TestCase simplest_pushPop
          , appendLabel "standalone_pushPop"  $ TestCase standalone_pushPop
          , appendLabel "standalone_pushPop2" $ TestCase RegressionTests.Issue5B.standalone_pushPop ]
          -- This is very ugly and should be unnecessary:
          ++ if plain then
               [ appendLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (CL.ChaseLevDeque a)) ]
             else []  
          ++ if wrapper then
               [ appendLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque CL.ChaseLevDeque a)) ]
             else []

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
test_parfib_work_stealing :: (DequeClass d, PopL d) => Elt -> IO (d Elt) -> IO ()
test_parfib_work_stealing origInput newqueue = do
  numAgents <- getNumAgents
  qs <- sequence (replicate numAgents newqueue)
  let arr = A.listArray (0,numAgents - 1) qs 
  
  let parfib !myQ !mySum !num
        | num <= 1  =
          do x <- tryPopL myQ
             case x of
               Nothing -> trySteal myQ (mySum+1)
               Just n  -> parfib myQ (mySum+1) n
        | otherwise = do 
          pushL myQ (num-1)
          parfib myQ mySum (num-2)
          
      trySteal myQ mySum =
        return mySum
       -- UNFINISHED...
  
  partial_sums <- forkJoin numAgents $ \ myId ->
    if myId == 0
    then parfib   (arr ! myId) 0 origInput
    else trySteal (arr ! myId) 0 
  
  return ()
