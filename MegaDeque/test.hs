{-# LANGUAGE ScopedTypeVariables, MagicHash #-}

import Data.Concurrent.Deque.Class
-- import Data.Concurrent.Deque.Class.Reference (newQueue)
import Data.Concurrent.MegaDeque 


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
