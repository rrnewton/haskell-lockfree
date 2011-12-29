{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
module Main where

{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}

import System.Exit
import Test.HUnit
import Control.Monad
import qualified Data.Concurrent.Deque.Tests as T
import qualified Data.Concurrent.Deque.Reference as R

main = 
 do putStrLn "Testing reference deque implementation."
    Counts{errors, failures} <- runTestTT $ T.test_fifo R.newQ 

    when (errors + failures > 0) $ do 
       putStrLn$ "Test.hs: Some tests failed! ("++show (errors+failures)++
		 ") Reporting non zero exit code..."
       exitFailure
