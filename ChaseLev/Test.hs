{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                  (defaultMain)
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Data.Concurrent.Deque.Tests     (tests_wsqueue)
import Data.Concurrent.Deque.ChaseLev  (ChaseLevDeque)
import Data.Concurrent.Deque.Class     (newQ)
import Data.Concurrent.Deque.Debugger  (DebugDeque)
-- import Data.Concurrent.Deque.ChaseLev  (newQ)

main =
  defaultMain$ hUnitTestToTests$
  -- test_all newQ
  tests_wsqueue
  (newQ :: IO (ChaseLevDeque a))
  -- (newQ :: IO (DebugDeque ChaseLevDeque a))
