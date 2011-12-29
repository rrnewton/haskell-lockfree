{-# LANGUAGE BangPatterns #-}
module Data.Concurrent.Deque.Tests 
 (
   test_wsqueue
 , runit
 )
 where 

import Data.Concurrent.Deque.Class as C

import Control.Monad
import Data.IORef
import System.Mem.StableName
import Text.Printf
import GHC.IO (unsafePerformIO)
import GHC.Conc
import Control.Concurrent.MVar

import qualified Data.Concurrent.Queue.MichaelScott as MS
import System.Environment



----------------------------------------------------------------------------------------------------
-- Test a plain FIFO queue:
----------------------------------------------------------------------------------------------------

-- | This test serially fills up a queue and then drains it.
test_fifo_filldrain :: DequeClass d => d Int -> IO Int
test_fifo_filldrain q = 
  do -- q <- newQ
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

-- | This one splits the 'numCapabilities' threads into producers and
-- consumers.  Each thread performs its designated operation as fast
-- as possible.  The 'Int' argument 'total' designates how many total
-- items should be communicated (irrespective of 'numCapabilities').
test_fifo_HalfToHalf :: DequeClass d => Int -> d Int -> IO ()
test_fifo_HalfToHalf total q = 
  do -- q <- newQ
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
     b <- nullQ q
     if b then return ()
          else error "Queue was not empty!!"

-- main = testCAS
-- runit = do 
--   putStrLn$ "Running test of Michael-Scott queues... "
--   args <- getArgs 
--   let size = case args of 
--               []  -> (1000 * 1000)
--               [n] -> (read n)
--   putStrLn$ "Putting "++show size++" elements through a queue...."
-- --  test_fifo2 size

-- | This creates an HUnit test list to perform all the tests above.
test_fifo


----------------------------------------------------------------------------------------------------
-- Test a Work-stealing queue:
test_wsqueue = undefined


----------------------------------------------------------------------------------------------------
-- Helpers

spinPop q = loop 1
 where 
  warnevery = 5000
  loop n = do
--     when (n `mod` warnevery == 0)
     when (n == warnevery)
	  (putStrLn$ "Warning: Failed to pop "++ show warnevery ++ " times consecutively.  That shouldn't happen in this benchmark.")
     x <- tryPopR q 
     case x of 
       Nothing -> loop (n+1)
       Just x  -> return (x, n)

----------------------------------------------------------------------------------------------------

