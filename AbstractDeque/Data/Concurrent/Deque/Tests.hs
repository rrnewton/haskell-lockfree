{-# LANGUAGE BangPatterns, RankNTypes #-}

-- | This module contains a battery of simple tests for queues
--   implementing the interface defined in
-- ` Data.Concurrent.Deque.Class`.

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Data.Concurrent.Deque.Tests 
 ( 
   -- * Tests for simple FIFOs.
   test_fifo_filldrain, test_fifo_OneBottleneck, test_fifo,

   -- * Tests for Work-stealing queues.
   test_ws_triv1, test_ws_triv2, test_wsqueue,

   -- * All deque tests, aggregated.
   test_all
 )
 where 

import Data.Concurrent.Deque.Class as C
import qualified Data.Concurrent.Deque.Reference as R

import Control.Monad
import Data.IORef
-- import System.Mem.StableName
import Text.Printf
import GHC.Conc (numCapabilities, throwTo, threadDelay, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent (yield, forkOS, forkIO, ThreadId)
import Control.Exception (catch, SomeException, fromException, AsyncException(ThreadKilled))
import System.Environment (getEnvironment)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit

import Debug.Trace (trace)

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

----------------------------------------------------------------------------------------------------
-- TODO: In addition to setting these parameters from environment
-- variables, it would be nice to route all of this through a
-- configuration record, so that it can be changed programmatically.

-- How many elements should each of the tests pump through the queue(s)?
numElems :: Int
numElems = case lookup "NUMELEMS" theEnv of 
             Nothing  -> 50 * 1000 -- 500000 
             Just str -> warnUsing ("NUMELEMS = "++str) $ 
                         read str

forkThread :: IO () -> IO ThreadId
forkThread = case lookup "OSTHREADS" theEnv of 
               Nothing -> forkIO
               Just x -> warnUsing ("OSTHREADS = "++x) $ 
                 case x of 
                   "0"     -> forkIO
                   "False" -> forkIO
                   "1"     -> forkOS
                   "True"  -> forkOS
                   oth -> error$"OSTHREAD environment variable set to unrecognized option: "++oth

-- | How many communicating agents are there?  By default one per
-- thread used by the RTS.
numAgents :: Int
numAgents = case lookup "NUMAGENTS" theEnv of 
             Nothing  -> numCapabilities
             Just str -> warnUsing ("NUMAGENTS = "++str) $ 
                         read str

-- | It is possible to have imbalanced concurrency where there is more
-- contention on the producing or consuming side (which corresponds to
-- settings of this parameter less than or greater than 1).
producerRatio :: Double
producerRatio = case lookup "PRODUCERRATIO" theEnv of 
                 Nothing  -> 1.0
                 Just str -> warnUsing ("PRODUCERRATIO = "++str) $ 
                             read str

warnUsing :: String -> a -> a
warnUsing str a = trace ("  [Warning]: Using environment variable "++str) a


----------------------------------------------------------------------------------------------------
-- Misc Helpers
----------------------------------------------------------------------------------------------------

myfork :: String -> IO () -> IO ThreadId
myfork msg = forkWithExceptions forkThread msg

-- Exceptions that walk up the fork tree of threads:
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do 
   parent <- myThreadId
   forkit $ 
      Control.Exception.catch action
	 (\ e -> 
          case fromException e of
            -- Let threadKilled exceptions through.
	    Just ThreadKilled -> return ()
            _ -> do
	      hPutStrLn stderr $ "Exception inside child thread "++show descr++": "++show e
	      throwTo parent (e::SomeException)
	 )


----------------------------------------------------------------------------------------------------
-- Test a plain FIFO queue:
----------------------------------------------------------------------------------------------------

-- | This test serially fills up a queue and then drains it.
test_fifo_filldrain :: DequeClass d => d Int -> IO ()
test_fifo_filldrain q = 
  do -- q <- newQ
     putStrLn "\nTest FIFO queue: sequential fill and then drain"
     putStrLn "==============================================="
--     let n = 1000
     let n = numElems
     putStrLn$ "Done creating queue.  Pushing elements:"
     forM_ [1..n] $ \i -> do 
       pushL q i
       when (i < 200) $ printf " %d" i
     putStrLn "\nDone filling queue with elements.  Now popping..."
     sumR <- newIORef 0
     forM_ [1..n] $ \i -> do
       (x,_) <- spinPopBkoff q 
       when (i < 200) $ printf " %d" x
       modifyIORef sumR (+x)
     s <- readIORef sumR
     let expected = sum [1..n] :: Int
     printf "\nSum of popped vals: %d should be %d\n" s expected
     when (s /= expected) (assertFailure "Incorrect sum!")
--     return s
     return ()


-- | This test splits the 'numAgents' threads into producers and
-- consumers which all communicate through a SINGLE queue.  Each
-- thread performs its designated operation as fast as possible.  The
-- 'Int' argument 'total' designates how many total items should be
-- communicated (irrespective of 'numAgents').
test_fifo_OneBottleneck :: DequeClass d => Bool -> Int -> d Int -> IO ()
test_fifo_OneBottleneck doBackoff total q = 
  do -- q <- newQ
     putStrLn$ "\nTest FIFO queue: producers & consumers thru 1 queue"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     putStrLn "======================================================"       
     mv <- newEmptyMVar          
     x <- nullQ q
     putStrLn$ "Check that queue is initially null: "++show x
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = max 1 (numAgents - producers)
	 perthread = total `quot` producers

     when (not doBackoff && (numCapabilities == 1 || numCapabilities < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
    
     forM_ [0..producers-1] $ \ id -> 
 	myfork "producer thread" $ 
          forM_ (take perthread [id * producers .. ]) $ \ i -> do 
	     pushL q i
             when (i - id*producers < 10) $ printf " [%d] pushed %d \n" id i

     printf "Forking %d consumer threads.\n" consumers

     forM_ [0..consumers-1] $ \ id -> 
 	myfork "consumer thread" $ do 

          let fn (!sum,!maxiters) i = do                
	       (x,iters) <- if doBackoff then spinPopBkoff q 
                                         else spinPopHard  q
	       when (i - id*producers < 10) $ printf " [%d] popped %d \n" id i
	       return (sum+x, max maxiters iters)
             
          pr <- foldM fn (0,0) (take perthread [id * producers .. ])
	  putMVar mv pr

     printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum (map fst ls)
     putStrLn$ "Consumers DONE.  Maximum retries for each consumer thread: "++ show (map snd ls)
     putStrLn$ "Final sum: "++ show finalSum
     putStrLn$ "Checking that queue is finally null..."
     b <- nullQ q
     if b then putStrLn$ "Sum matched expected, test passed."
          else assertFailure "Queue was not empty!!"

-- | This test uses a separate queue per consumer thread.  The queues
-- are used in a single-writer multiple-reader fashion (mailboxes).
          
-- test_fifo_mailboxes

------------------------------------------------------------

-- | This creates an HUnit test list to perform all the tests above.
test_fifo :: DequeClass d => (forall elt. IO (d elt)) -> Test
test_fifo newq = TestList 
  [
    TestLabel "test_fifo_filldrain"  (TestCase$ assert $ newq >>= test_fifo_filldrain)
    -- Do half a million elements by default:
  , TestLabel "test_fifo_OneBottleneck_backoff"    (TestCase$ assert $ newq >>= test_fifo_OneBottleneck True  numElems)
--  , TestLabel "test_fifo_OneBottleneck_aggressive" (TestCase$ assert $ newq >>= test_fifo_OneBottleneck False numElems)
--  , TestLabel "test the tests" (TestCase$ assert $ assertFailure "This SHOULD fail.")
  ]


----------------------------------------------------------------------------------------------------
-- Test a Work-stealing queue:
----------------------------------------------------------------------------------------------------

-- | Trivial test: push then pop.
test_ws_triv1 :: PopL d => d [Char] -> IO ()
test_ws_triv1 q = do
  pushL q "hi" 
  Just x <- tryPopL q 
  assertEqual "test_ws_triv1" x "hi"

-- | Trivial test: push left, pop left and right.
test_ws_triv2 :: PopL d => d [Char] -> IO ()
test_ws_triv2 q = do
  pushL q "one" 
  pushL q "two" 
  pushL q "three" 
  pushL q "four" 
  ls <- sequence [tryPopR q, tryPopR q, 
		  tryPopL q, tryPopL q,
		  tryPopL q, tryPopR q ]
  assertEqual "test_ws_triv2" ls 
    [Just "one",Just "two",Just "four",Just "three",Nothing,Nothing]


-- | Aggregate tests for work stealing queues.
test_wsqueue :: (PopL d) => (forall elt. IO (d elt)) -> Test
test_wsqueue newq = TestList
 [
   TestLabel "test_ws_triv1"  (TestCase$ assert $ newq >>= test_ws_triv1)
 , TestLabel "test_ws_triv2"  (TestCase$ assert $ newq >>= test_ws_triv2)
 ]

----------------------------------------------------------------------------------------------------
-- Combine all tests -- for a deques supporting all capabilities.
----------------------------------------------------------------------------------------------------

test_all :: (PopL d) => (forall elt. IO (d elt)) -> Test
test_all newq = 
  TestList 
   [ test_fifo    newq
   , test_wsqueue newq
   ]

----------------------------------------------------------------------------------------------------
-- Helpers

spinPopBkoff q = loop 1
 where 
  hardspinfor = 10
  sleepevery = 1000
  warnafter  = 5000
  errorafter = 1 * 1000 * 1000
  loop n = do
     when (n == warnafter)
	  (putStrLn$ "Warning: Failed to pop "++ show warnafter ++ 
	             " times consecutively.  That shouldn't happen in this benchmark.")
--     when (n == errorafter) (error "This can't be right.  A queue consumer spun 1M times.")
     x <- tryPopR q 
     case x of 
       -- This yields EVERY time.  And yet we get these real bursts / runs of failure.
       Nothing -> do 
                     -- Every `sleepevery` times do a significant delay:
		     if n `mod` sleepevery == 0 
		      then do putStr "!"
                              threadDelay n -- 1ms after 1K fails, 2 after 2K...
		      else when (n > hardspinfor) $ do 
                             putStr "."
			     yield -- At LEAST yield... you'd think this is pretty strong backoff.
		     loop (n+1)
       Just x  -> return (x, n)


-- | This is always a crazy and dangerous thing to do -- GHC cannot
--   deschedule this spinning thread because it does not allocate:
spinPopHard q = loop 1
 where 
  loop n = do
     x <- tryPopR q 
     case x of 
       Nothing -> do loop (n+1)
       Just x  -> return (x, n)



----------------------------------------------------------------------------------------------------

