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

main = main1

main1 = do 
  putStrLn$ "Running with numElems "++show numElems++" and numAgents "++ show numAgents
  putStrLn "Use NUMELEMS and +RTS to control the size of this benchmark."
  args <- getArgs
  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $   
    defaultMain$ hUnitTestToTests$
    TestList
    [ TestLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (CL.ChaseLevDeque a))
    , TestLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
    , TestLabel "newtriv"  $ TestCase main2
    ]

-- main2 = new_triv1 =<< (newQ :: IO (DebugDeque ChaseLevDeque a))
main2 = do
  q <- (newQ :: IO (DebugDeque CL.ChaseLevDeque a))
  pushL q "hi" 
  Just x <- tryPopL q 
  assertEqual "test_ws_triv1" x "hi"
  putStrLn "Test passed."
  
-- main2 = new_triv1 =<< (newQ :: IO (ChaseLevDeque a))
-- main2 = new_triv1 =<< (newQ :: IO (SimpleDeque a))

-- <Small Reproducer>
-- This is what's failing with the debug wrapper, WHY?
new_triv1 :: PopL d => d [Char] -> IO ()
new_triv1 q = do
  pushL q "hi" 
  Just x <- tryPopL q 
  assertEqual "test_ws_triv1" x "hi"

