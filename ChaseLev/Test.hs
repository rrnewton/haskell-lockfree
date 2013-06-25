{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import Control.Concurrent (setNumCapabilities, getNumCapabilities)
import GHC.Conc (getNumProcessors)
import Control.Exception (bracket)
import Data.Concurrent.Deque.Tests     (tests_wsqueue, numElems, numAgents)
import Data.Concurrent.Deque.Reference (SimpleDeque)
import Data.Concurrent.Deque.Class     -- (newQ)
import Data.Concurrent.Deque.Debugger  (DebugDeque)
import qualified Data.Concurrent.Deque.ChaseLev as CL
import qualified Data.Set as S
-- import Data.Concurrent.Deque.ChaseLev  (newQ)
import System.Environment (withArgs, getArgs)
import Test.HUnit as HU
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Text.Printf (printf)

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
                    [1, 2, np `quot` 2, np, 2*np ]
  putStrLn $"Running all tests for these thread settings: "  ++show all_threads

  let all_tests :: HU.Test
      all_tests = TestList
        [ TestLabel "simplest_pushPop"  $ TestCase simplest_pushPop
        , TestLabel "standalone_pushPop"  $ TestCase standalone_pushPop
        , TestLabel "standalone_pushPop2" $ TestCase RegressionTests.Issue5B.standalone_pushPop      
        , TestLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (CL.ChaseLevDeque a))
        , TestLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
        ]
        
  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $   
    TF.defaultMain$ hUnitTestToTests $ TestList $    
       [ setThreads n all_tests | n <- all_threads ]


-- | Dig through the test constructors to find the leaf IO actions and bracket them
--   with a thread-setting action.
setThreads :: Int -> HU.Test -> HU.Test
setThreads nm tst = loop False tst
 where
   loop flg x = 
    case x of
      TestLabel lb t2 -> TestLabel (decor flg lb) (loop True t2)
      TestList ls -> TestList (map (loop flg) ls)
      TestCase io -> TestCase (bracketThreads nm io)

   -- We only need to insert the numcapabilities in the description string ONCE:
   decor False lb = "N"++show nm++"_"++ lb
   decor True  lb = lb

bracketThreads :: Int -> IO a -> IO a
bracketThreads n act =
  bracket (getNumCapabilities)
          setNumCapabilities
          (\_ -> do printf "\n   [Setting # capabilities to %d before test] \n" n
                    setNumCapabilities n
                    act)

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

