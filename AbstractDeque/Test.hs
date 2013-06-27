{-# LANGUAGE CPP, ScopedTypeVariables, NamedFieldPuns #-}
#if __GLASGOW_HASKELL >= 700
{-# OPTIONS_GHC -with-rtsopts=-K32M #-}
#endif

import Data.Concurrent.Deque.Class
-- import Data.Concurrent.Deque.Class.Reference (newQueue)
-- import Data.Concurrent.MegaDeque 

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit     (hUnitTestToTests)
import Test.HUnit (assert, assertEqual, Test(TestCase, TestList, TestLabel)) 
import qualified Data.Concurrent.Deque.Tests as T
import qualified Data.Concurrent.Deque.Reference as R
import qualified Data.Concurrent.Deque.Class as C
import Data.Concurrent.Deque.Debugger (DebugDeque)
import System.Environment (withArgs)

-- Import the instances:
import Data.Concurrent.Deque.Reference.DequeInstance ()

test_1 :: Test
test_1 = TestCase $ assert $ 
  do q <- R.newQ -- Select a specific implementation.
     pushR q 3
     Just x <- tryPopR q
     assertEqual "test_1 result" x (3::Integer)

test_2 :: Test
test_2 = TestCase $ assert $ 
  do 
     -- Here's an example of type-based restriction of the queue implementation:
     q <- newQ :: IO (Deque NT T D S Bound Safe Int)
     pushL q 33
--     pushR q 33  -- This would cause a type error because the Right end is not Double-capable.
     Just x <- tryPopR q
     assertEqual "test_2 result" x 33

main :: IO ()
#if __GLASGOW_HASKELL__ >= 700
main = T.stdTestHarness $ return all_tests
 where 
 all_tests :: Test
 all_tests = 
   TestLabel "Reference_Deque" $ TestList $ 
     [ TestLabel "test_1" test_1
     , TestLabel "test_2" test_2 
     , TestLabel "direct"$ T.tests_all R.newQ
     -- Test going through the class interface as well:  
     , TestLabel "thru_class"$ T.tests_all (C.newQ :: IO (R.SimpleDeque a))
     , TestLabel "with_debug"$ T.tests_all (C.newQ :: IO (DebugDeque R.SimpleDeque a))
     ]

-- main = do 
--   putStrLn "[ Test executable: test reference deque implementation... ]"
--   withArgs ["-j1","--jxml=test-results.xml"] $   
--     defaultMain$ hUnitTestToTests$
 
#else
main = putStrLn "WARNING: Tests disabled for GHC < 7"
#endif
