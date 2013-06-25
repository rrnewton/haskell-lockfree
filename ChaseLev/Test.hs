{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import Control.Concurrent (setNumCapabilities, getNumCapabilities)
import GHC.Conc (getNumProcessors)
import Data.Concurrent.Deque.Tests     (tests_wsqueue, numElems, numAgents)
import Data.Concurrent.Deque.Reference (SimpleDeque)
import Data.Concurrent.Deque.Class     -- (newQ)
import Data.Concurrent.Deque.Debugger  (DebugDeque)
import qualified Data.Concurrent.Deque.ChaseLev as CL
import qualified Data.Set as S
-- import Data.Concurrent.Deque.ChaseLev  (newQ)
import System.Environment (withArgs, getArgs)
import Test.HUnit 
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit  (hUnitTestToTests)

import RegressionTests.Issue5 (standalone_pushPop)
import qualified RegressionTests.Issue5B 

main :: IO ()
main = do 
  putStrLn$ "Running with numElems "++show numElems++" and numAgents "++ show numAgents
  putStrLn "Use NUMELEMS and +RTS to control the size of this benchmark."
  args <- getArgs

  np <- getNumProcessors
  putStrLn $"Running on a machine with "++show np++" hardware threads."  
  let all_threads = S.toList$ S.fromList$
                    [1, 2, np `quot` 2, np-1, np, np+1, 2*np ]
  putStrLn $"Running all tests for these thread settings: "  ++show all_threads

  let all_tests = TestList
        [ TestLabel "simplest_pushPop"  $ TestCase simplest_pushPop
        , TestLabel "standalone_pushPop"  $ TestCase standalone_pushPop
        , TestLabel "standalone_pushPop2" $ TestCase RegressionTests.Issue5B.standalone_pushPop      
        , TestLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (CL.ChaseLevDeque a))
        , TestLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
        ]
        
  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $   
    TF.defaultMain$ 
       [ withThreads n t
       | n <- all_threads
       , t <- hUnitTestToTests all_tests ]

withThreads :: Int -> TF.Test -> TF.Test
withThreads n tst = do
  TF.buildTest $
    do putStrLn$ "\n   [Setting # capabilities to "++show n++" before test] "
       putStrLn    "   ==================================================== "
       setNumCapabilities n
       return tst
  
  -- TF.buildTestBracketed $
  --   do orig <- getNumCapabilities
  --      setNumCapabilities n
  --      return (tst, setNumCapabilities orig)


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

