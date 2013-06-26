{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import Control.Concurrent (setNumCapabilities, getNumCapabilities)
import GHC.Conc (getNumProcessors)
import Control.Exception (bracket)
import qualified Data.Set as S
-- import Data.Concurrent.Deque.ChaseLev  (newQ)
import System.Environment (withArgs, getArgs, getEnvironment)
import Test.HUnit as HU
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Text.Printf (printf)

import Data.Concurrent.Deque.Tests     
import Data.Concurrent.Deque.Class
import Data.Concurrent.Deque.Debugger  (DebugDeque)
import qualified Data.Concurrent.Deque.ChaseLev as CL

import RegressionTests.Issue5 (standalone_pushPop)
import qualified RegressionTests.Issue5B 

main :: IO ()
main = stdTestHarness $ do 
  theEnv <- getEnvironment
  let wrapper = case lookup "NOWRAPPER" theEnv of
                 Just _  -> False
                 Nothing -> True
  let plain = case lookup "ONLYWRAPPER" theEnv of
                Just _  -> False
                Nothing -> True  
  let all_tests :: HU.Test
      all_tests = TestList $ 
        [ TestLabel "simplest_pushPop"  $ TestCase simplest_pushPop
        , TestLabel "standalone_pushPop"  $ TestCase standalone_pushPop
        , TestLabel "standalone_pushPop2" $ TestCase RegressionTests.Issue5B.standalone_pushPop ]
        -- This is very ugly and should be unnecessary:
        ++ if plain then
             [ TestLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (CL.ChaseLevDeque a)) ]
           else []  
        ++ if wrapper then
             [ TestLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque CL.ChaseLevDeque a)) ]
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
              Just x -> x
              Nothing -> error "Even a single push/pop in isolation did not work!"
     assertEqual "test_ws_triv1" y "hi"

