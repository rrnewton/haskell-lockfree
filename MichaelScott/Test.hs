{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                     (defaultMain)
import Test.Framework.Providers.HUnit     (hUnitTestToTests)
import Data.Concurrent.Deque.Tests        (tests_fifo)
import Data.Concurrent.Queue.MichaelScott (newQ)
import System.Environment (withArgs)
import Test.HUnit (Test(TestLabel))

main =
  withArgs ["-j1","--jxml=test-results.xml"] $ 
  defaultMain$ hUnitTestToTests$
  TestLabel "MichaelScott" $
  tests_fifo newQ
