{-# LANGUAGE BangPatterns #-}
module Main where

{- Example build:
  ghc -DNATIVE_CAS --make Test.hs -o Test.exe -rtsopts -fforce-recomp
-}

import Control.Monad
import Data.IORef
import System.Mem.StableName
import Text.Printf
import GHC.IO (unsafePerformIO)
import GHC.Conc
import Control.Concurrent.MVar

-- import Data.CAS (casIORef)
-- import Data.CAS.Fake (casIORef)

import Data.Concurrent.Queue.MichaelScott
import System.Environment

spinPop q = loop 1
 where 
  warnevery = 1000
  loop n = do
--     when (n `mod` warnevery == 0)
     when (n == warnevery)
	  (putStrLn$ "Warning: Failed to pop "++ show warnevery ++ " times consecutively.  That shouldn't happen in this benchmark.")
     x <- tryPopR q 
     case x of 
       Nothing -> loop (n+1)
       Just x  -> return (x, n)

testQ1 = 
  do q <- newQ
     let n = 1000
     putStrLn$ "Done creating queue.  Pushing elements:"
     forM_ [1..n] $ \i -> do 
       pushL q i
       printf " %d" i
     putStrLn "\nDone filling queue with elements.  Now popping..."
     sumR <- newIORef 0
     forM_ [1..n] $ \i -> do
       (x,_) <- spinPop q 
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
  do q <- newQ
     mv <- newEmptyMVar     
     
     x <- nullQ q
     putStrLn$ "Check that queue is initially null: "++show x
     let producers = max 1 (numCapabilities `quot` 2)
	 consumers = producers
	 perthread = total `quot` producers

     printf "Forking %d producer threads.\n" producers 
    
     forM_ [0..producers-1] $ \ id -> 
 	forkIO $ 
          forM_ (take perthread [id * producers .. ]) $ \ i -> do 
	     pushL q i
             when (i - id*producers < 10) $ printf " [%d] pushed %d \n" id i

     printf "Forking %d consumer threads.\n" consumers

     forM_ [0..consumers-1] $ \ id -> 
 	forkIO $ do 

          let fn (!sum,!maxiters) i = do
	       (x,iters) <- spinPop q 
	       when (i - id*producers < 10) $ printf " [%d] popped %d \n" id i
	       return (sum+x, max maxiters iters)
             
          pr <- foldM fn (0,0) (take perthread [id * producers .. ])
	  putMVar mv pr

     printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum (map fst ls)
     putStrLn$ "Maximum retries for each consumer thread: "++ show (map snd ls)
     putStrLn$ "Final sum: "++ show finalSum
     putStrLn$ "Checking that queue is finally null..."
     print =<< nullQ q
     return ()

-- main = testCAS
main = do 
  putStrLn$ "Running test of Michael-Scott queues using: "++ cas_version
  args <- getArgs 
  let size = case args of 
              []  -> (1000 * 1000)
              [n] -> (read n)
  putStrLn$ "Putting "++show size++" elements through a queue...."
  testQ2 size

-- main = testQ2 (10)
