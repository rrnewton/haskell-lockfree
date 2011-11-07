{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.IORef
import System.Mem.StableName
import Text.Printf
import GHC.IO (unsafePerformIO)
import GHC.Conc
import Control.Concurrent.MVar

import Data.CAS (casIORef)

import Data.Concurrent.LinkedQueue

spinPop q = do
  x <- tryPop q 
  case x of 
    Nothing -> spinPop q
    Just x  -> return x

testQ1 = 
  do q <- newLinkedQueue 
     let n = 1000
     putStrLn$ "Done creating queue.  Pushing elements:"
     forM_ [1..n] $ \i -> do 
       push q i
       printf " %d" i
     putStrLn "\nDone filling queue with elements.  Now popping..."
     sumR <- newIORef 0
     forM_ [1..n] $ \i -> do
       x <- spinPop q 
       printf " %d" x
       modifyIORef sumR (+x)
     s <- readIORef sumR
     let expected = sum [1..n] :: Int
     printf "\nSum of popped vals: %d should be %d\n" s expected
     when (s /= expected) (error "Incorrect sum!")
     return s

-- This one splits the numCapabilities threads into producers and consumers
testQ2 :: Int -> IO ()
testQ2 total = 
  do q <- newLinkedQueue
     mv <- newEmptyMVar     
     let producers = max 1 (numCapabilities `quot` 2)
	 consumers = producers
	 perthread = total `quot` producers

     printf "Forking %d producer threads.\n" producers 
    
     forM_ [0..producers-1] $ \ id -> 
 	forkIO $ 
          forM_ (take perthread [id * producers .. ]) $ \ i -> do 
	     push q i
             printf " [%d] pushed %d \n" id i

     printf "Forking %d consumer threads.\n" consumers

     forM_ [0..consumers-1] $ \ id -> 
 	forkIO $ do 
          sum <- newIORef 0
          forM_ (take perthread [id * producers .. ]) $ \ i -> do
	     x <- spinPop q 
             printf " [%d] popped %d \n" id i
	     modifyIORef sum (+x)
	  s <- readIORef sum
	  putMVar mv s

     printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum ls
     putStrLn$ "Final sum: "++ show finalSum
     return ()

-- main = testCAS
main = testQ2 (1000 * 1000)
-- main = testQ2 (10)
