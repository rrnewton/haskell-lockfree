{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where

import Test.HUnit as HU
import Data.Concurrent.Deque.Tests 
import Data.Concurrent.Deque.Class
import Data.Concurrent.MegaDeque () -- Instances.

main :: IO ()
main = stdTestHarness $ return all_tests
 where 
 all_tests :: HU.Test
 all_tests = TestList $ 
   [ TestLabel "WSDeque"  $ tests_wsqueue  (newQ :: IO (WSDeque a))
   , TestLabel "TS_Queue" $ tests_fifo     (newQ :: IO (ConcQueue a))
   , TestLabel "NT_Queue" $ tests_fifo     (newQ :: IO (Queue a))
   , TestLabel "Full_TS_Deque" $ tests_all (newQ :: IO (ConcDeque a))
--   , TestLabel "Maxed" $ tests_all       (newQ :: IO (Deque T T D D Grow Safe))
   ]
