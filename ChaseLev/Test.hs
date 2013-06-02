{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                  (defaultMain)
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Data.Concurrent.Deque.Tests     (tests_wsqueue, numElems, numAgents)
import Data.Concurrent.Deque.ChaseLev  (ChaseLevDeque)
import Data.Concurrent.Deque.Class     (newQ)
import Data.Concurrent.Deque.Debugger  (DebugDeque)
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
    [
      -- TestLabel "ChaseLev" $ tests_wsqueue (newQ :: IO (ChaseLevDeque a))
      TestLabel "ChaseLev(DbgWrapper)" $ tests_wsqueue (newQ :: IO (DebugDeque ChaseLevDeque a))
    ]


