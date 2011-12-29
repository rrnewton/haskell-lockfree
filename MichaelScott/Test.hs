{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                     (defaultMain)
import Test.Framework.Providers.HUnit     (hUnitTestToTests)
import Data.Concurrent.Deque.Tests        (test_fifo)
import Data.Concurrent.Queue.MichaelScott (newQ)

main = defaultMain$ hUnitTestToTests$ test_fifo newQ 
