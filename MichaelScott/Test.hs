{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                     (defaultMain)
import Test.Framework.Providers.HUnit     (hUnitTestToTests)
import Data.Concurrent.Deque.Tests        (tests_fifo, numElems, getNumAgents)
import System.Environment (getArgs, withArgs)
import Test.HUnit 

-- import Data.Concurrent.Queue.MichaelScott (newQ)
import Data.Concurrent.Queue.MichaelScott (LinkedQueue)
import Data.Concurrent.Deque.Class        (newQ)
import Data.Concurrent.Deque.Debugger

main = do
  numAgents <- getNumAgents
  putStrLn$ "Running with numElems "++show numElems++" and numAgents "++ show numAgents
  putStrLn "Use NUMELEMS and +RTS to control the size of this benchmark."
  args <- getArgs
  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $ 
    defaultMain$ hUnitTestToTests$
    TestList
    [ TestLabel "MichaelScott" $ tests_fifo (newQ :: IO (LinkedQueue a))
    , TestLabel "MichaelScott(DbgWrapper)" $
        tests_fifo (newQ :: IO (DebugDeque LinkedQueue a))
    ]

  
