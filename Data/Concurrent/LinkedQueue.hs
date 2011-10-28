{-# LANGUAGE BangPatterns #-}

-- module Data.Concurrent.LinkedQueue 
module Main

  where

import Data.CAS
import Data.IORef

-- Considering using the Queue class definition:
-- import Data.MQueue.Class

-- | A straightforward implementation of classic Michael & Scott Queues.


data LinkedQueue a = LQ 
    { head :: Pair a
    , tail :: Pair a
    }

data Pair a = Null | Cons a (IORef (LinkedQueue a))

push :: LinkedQueue a -> a  -> IO ()
push = undefined

tryPop ::  LinkedQueue a -> IO (Maybe a)
tryPop = undefined

newLinkedQueue :: IO (LinkedQueue a)
newLinkedQueue = do 
  r <- newIORef 
  let newp = Cons undefined r
  return (LQ newp newp)

--------------------------------------------------------------------------------
-- Scrap:

casStrict r !o !n = casIORef r o n

testCAS = 
  do let zer = (0::Int)
     r <- newIORef zer
     let loop 0 = return ()
	 loop n = do
          (b,v) <- casIORef r zer 100  -- Must use "zer" here.
--          (b,v) <- casStrict r 0 100  -- Otherwise this is nondeterministic based on compiler opts.
		   -- Sometimes the latter version works on the SECOND evaluation of testCAS.  Interesting.
          putStrLn$ "After CAS " ++ show (b,v)
          loop (n-1)
     loop 10 
     return ()


main = testCAS
