{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                  (defaultMain)
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Data.Concurrent.Deque.Tests     (tests_wsqueue, numElems, numAgents)
import Data.Concurrent.Deque.Reference (SimpleDeque)
import Data.Concurrent.Deque.Class     -- (newQ)
import Data.Concurrent.Deque.Debugger  (DebugDeque)
import qualified Data.Concurrent.Deque.ChaseLev as CL
-- import Data.Concurrent.Deque.ChaseLev  (newQ)
import System.Environment (withArgs, getArgs)
import Test.HUnit 

main :: IO ()
main = do 
  putStrLn$ "Running with numElems "++show numElems++" and numAgents "++ show numAgents
  putStrLn "Use NUMELEMS and +RTS to control the size of this benchmark."
  args <- getArgs
  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $   
    defaultMain$ hUnitTestToTests$
    TestList
    [ TestLabel "simplest_single_CAS"  $ TestCase simplest_single_CAS
    , TestLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (CL.ChaseLevDeque a))
    , TestLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
    ]

--------------------------------------------------------------------------------
-- Individual unit and regression tests:
-------------------------------------------------------------------------------


-- <Small Reproducer for recent debug wrapper problem>
simplest_single_CAS :: IO ()
simplest_single_CAS =
  triv =<< (newQ :: IO (DebugDeque CL.ChaseLevDeque a))           
 where   
   -- This is what's failing with the debug wrapper, WHY?
   triv :: PopL d => d [Char] -> IO ()
   triv q = do
     pushL q "hi" 
     x <- tryPopL q
     let y = case x of
              Just x -> x
              Nothing -> error "Even a single CAS in isolation did not work!"
     assertEqual "test_ws_triv1" y "hi"
