{-# LANGUAGE CPP, ScopedTypeVariables, NamedFieldPuns #-}
#if __GLASGOW_HASKELL >= 700
{-# OPTIONS_GHC -with-rtsopts=-K32M #-}
#endif

import Data.Concurrent.Deque.Class
-- import Data.Concurrent.Deque.Class.Reference (newQueue)
-- import Data.Concurrent.MegaDeque 

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit     (hUnitTestToTests)
import Test.HUnit (assert, assertEqual, Test(TestCase, TestList))
import qualified Data.Concurrent.Deque.Tests as T
import qualified Data.Concurrent.Deque.Reference as R

-- Import the instances:
import qualified Data.Concurrent.Deque.Reference.DequeInstance 
import System.Exit

test_1 = TestCase $ assert $ 
  do q <- R.newQ -- Select a specific implementation.
     pushR q 3
     Just x <- tryPopR q
     assertEqual "test_1 result" x 3

test_2 = TestCase $ assert $ 
  do 
     -- Here's an example of type-based restriction of the queue implementation:
     q <- newQ :: IO (Deque NT T D S Bound Safe Int)
     pushL q 33
--     pushR q 33  -- This would cause a type error because the Right end is not Double-capable.
     Just x <- tryPopR q
     assertEqual "test_2 result" x 33

#if __GLASGOW_HASKELL__ >= 700
main = do 
  putStrLn "[ Test executable: test reference deque implementation... ]"
  defaultMain$ hUnitTestToTests$ 
      TestList $ 
        [ 
          T.test_all R.newQ 
        , test_1
	, test_2
        ]
#else
main = putStrLn "WARNING: Tests disabled for GHC < 7"
#endif
