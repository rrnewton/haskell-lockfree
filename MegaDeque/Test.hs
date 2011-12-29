{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
{- Example build:
  ghc --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}
module Main where
import Test.Framework                  (defaultMain)
import Test.HUnit                      
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Data.Concurrent.Deque.Tests     (test_all, test_fifo)
import Data.Concurrent.Deque.Class     (newQ, WSDeque, Queue)
import Data.Concurrent.MegaDeque       ()


-- Constrain the type:
-- mynew :: IO (WSDeque elt)
-- mynew = newQ
-- Deque Nonthreadsafe Threadsafe DoubleEnd SingleEnd Grow Safe a

main = defaultMain$ hUnitTestToTests$ TestList 
         -- Constrain the type to select an instance:
        [ test_all  (newQ :: IO (WSDeque a))
        , test_fifo (newQ :: IO (Queue a))
        ]

{-

-- test = 
-- --  do q <- newQueue
--   do q <- newQueue
--      pushR q 3
--      x <- tryPopR q
--      print x
--      return q


test = do 
     q :: Deque NT T D S Bound Safe Int <- newQ -- This will not use LinkedQueue
     putStrLn "First pushing an element to the fallback and popping it:"
     pushL q 33
     x <- tryPopR q
     print x

     q2 :: Deque NT T SingleEnd SingleEnd Bound Safe Int <- newQ -- Can use LinkedQueue
     putStrLn "Second pushing an element to a LinkedQueue and popping it:"
     pushL q2 33
     x <- tryPopR q2
     print x

     return (q,q2)


-- Note: I just got a SEGFAULT in GHCI on this one, EVEN THOUGH
-- LinkedQueue was using the "Fake" CAS.

-- That's just on MAC OS though..

main = test

-}