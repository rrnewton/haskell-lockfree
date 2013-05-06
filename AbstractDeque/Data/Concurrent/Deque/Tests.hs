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
   test_all,

   -- * Testing parameters
   numElems, numAgents, producerRatio
 )
 where 

import Data.Concurrent.Deque.Class as C
import qualified Data.Concurrent.Deque.Reference as R

import Control.Monad
-- import qualified Data.Vector V
-- import qualified Data.Vector.Mutable as MV
import Data.Array as A
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
import System.Random (randomRIO)
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
             Nothing  -> 100 * 1000 -- 500000
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
-- Test a plain FIFO queue:
----------------------------------------------------------------------------------------------------

-- | This test serially fills up a queue and then drains it.
test_fifo_filldrain :: DequeClass d => d Int -> IO ()
test_fifo_filldrain q = 
  do
     putStrLn "\nTest FIFO queue: sequential fill and then drain"
     putStrLn "==============================================="
--     let n = 1000
     let n = numElems
     putStrLn$ "Done creating queue.  Pushing elements:"
     forM_ [1..n] $ \i -> do 
       pushL q i
       when (i < 200) $ printf " %d" i
     putStrLn "\nDone filling queue with elements.  Now popping..."
     
     let loop 0 !sumR = return sumR
         loop i !sumR = do 
           (x,_) <- spinPopBkoff q 
           when (i < 200) $ printf " %d" x
           loop (i-1) (sumR + x)
     s <- loop n 0
     let expected = sum [1..n] :: Int
     printf "\nSum of popped vals: %d should be %d\n" s expected
     when (s /= expected) (assertFailure "Incorrect sum!")
     return ()

{-# INLINABLE test_fifo_OneBottleneck #-}
-- | This test splits the 'numAgents' threads into producers and
-- consumers which all communicate through a SINGLE queue.  Each
-- thread performs its designated operation as fast as possible.  The
-- 'Int' argument 'total' designates how many total items should be
-- communicated (irrespective of 'numAgents').
test_fifo_OneBottleneck :: DequeClass d => Bool -> Int -> d Int -> IO ()
test_fifo_OneBottleneck doBackoff total q = 
  do
     assertBool "test_fifo_OneBottleneck requires thread safe left end"  (leftThreadSafe q)
     assertBool "test_fifo_OneBottleneck requires thread safe right end" (rightThreadSafe q)
    
     putStrLn$ "\nTest FIFO queue: producers & consumers thru 1 queue"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     putStrLn "======================================================"       

     x <- nullQ q
     putStrLn$ "Check that queue is initially null: "++show x
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = max 1 (numAgents - producers)
	 perthread  = total `quot` producers
         perthread2 = total `quot` consumers

     when (not doBackoff && (numCapabilities == 1 || numCapabilities < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
     printf "Forking %d consumer threads, each consuming %d elements.\n" consumers perthread2
    
     forM_ [0..producers-1] $ \ id -> 
 	myfork "producer thread" $ do
          let start = id*perthread 
          printf "  * Producer thread %d pushing ints from %d to %d \n" id start (start+perthread - 1)
          for_ start (start+perthread) $ \ i -> do 
	     pushL q i
             when (i < start + 10) $ printf " [p%d] pushed %d \n" id i

     ls <- forkJoin consumers $ \ id -> 
          let consume_loop sum maxiters i | i == perthread2 = return (sum, maxiters)
              consume_loop !sum !maxiters i = do
                (x,iters) <- if doBackoff then spinPopBkoff q 
                                          else spinPopHard  q
                when (i >= perthread2 - 10) $ printf " [c%d] popped #%d = %d \n" id i x
                consume_loop (sum+x) (max maxiters iters) (i+1)
          in consume_loop 0 0 0

     let finalSum = Prelude.sum (map fst ls)
     putStrLn$ "Consumers DONE.  Maximum retries for each consumer thread: "++ show (map snd ls)
     putStrLn$ "Final sum: "++ show finalSum
     assertEqual "Correct final sum" (expectedSum total) finalSum
     putStrLn$ "Checking that queue is finally null..."
     b <- nullQ q
     if b then putStrLn$ "Sum matched expected, test passed."
          else assertFailure "Queue was not empty!!"

-- | This test uses a separate queue per consumer thread.  The queues
-- are used in a single-writer multiple-reader fashion (mailboxes).
test_contention_free_parallel :: DequeClass d => Bool -> Int -> IO (d Int) -> IO ()
test_contention_free_parallel doBackoff total newqueue = 
  do 
     putStrLn$ "\nTest FIFO queue: producers & consumers thru N queues"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     putStrLn "======================================================"       
     mv <- newEmptyMVar
          
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = producers -- max 1 (numAgents - producers)
	 perthread  = total `quot` producers
         perthread2 = total `quot` consumers

     qs <- sequence (replicate consumers newqueue)
     
     when (not doBackoff && (numCapabilities == 1 || numCapabilities < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
     printf "Forking %d consumer threads, each consuming %d elements.\n" consumers perthread2
    
     forM_ (zip [0..producers-1] qs) $ \ (id, q) -> 
 	myfork "producer thread" $
          let start = id*perthread in
          for_ 0 perthread $ \ i -> do 
	     pushL q i
             when (i - id*producers < 10) $ printf " [%d] pushed %d \n" id i

     forM_ (zip [0..consumers-1] qs) $ \ (id, q) -> 
 	myfork "consumer thread" $ do 
          let consume_loop sum maxiters i | i == perthread = return (sum, maxiters)
              consume_loop !sum !maxiters i = do
                (x,iters) <- if doBackoff then spinPopBkoff q 
                                          else spinPopHard  q
                when (i < 10) $ printf " [%d] popped #%d = %d \n" id i x
                unless (x == i) $ error $ "Message out of order! Expected "++show i++" recevied "++show x
                consume_loop (sum+x) (max maxiters iters) (i+1)
          pr <- consume_loop 0 0 0
	  putMVar mv pr

     printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum (map fst ls)
     putStrLn$ "Consumers DONE.  Maximum retries for each consumer thread: "++ show (map snd ls)
     putStrLn$ "All messages received in order.  Final sum: "++ show finalSum
     assertEqual "Correct final sum" (producers * expectedSum perthread) finalSum          
     putStrLn$ "Checking that queue is finally null..."
     bs <- mapM nullQ qs
     if all id bs
       then putStrLn$ "Sum matched expected, test passed."
       else assertFailure "Queue was not empty!!"



-- | This test uses a number of producer and consumer threads which push and pop
-- elements from random positions in an array of FIFOs.
test_random_array_comm :: DequeClass d => Int -> Int -> IO (d Int) -> IO ()
test_random_array_comm size total newqueue | size > 0 = do

   qs <- sequence (replicate size newqueue)
--   arr <- V.thaw $ V.fromlist qs
   let arr = A.listArray (0,size-1) qs

   assertBool "test_random_array_comm requires thread safe left end"  (leftThreadSafe (head qs))
   assertBool "test_random_array_comm requires thread safe right end" (rightThreadSafe (head qs))
   
   putStrLn$ "\nTest FIFO queue: producers & consumers select random queues"
   putStrLn "======================================================"       

   let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
       consumers = producers -- max 1 (numAgents - producers)
       perthread  = total `quot` producers
       perthread2 = total `quot` consumers

   qs <- sequence (replicate consumers newqueue)

   printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
   printf "Forking %d consumer threads, each consuming %d elements.\n" consumers perthread2

   forM_ [0..producers-1] $ \ id -> 
      myfork "producer thread" $
        let start = id*perthread in
        for_ 0 perthread $ \ i -> do
           -- Randomly pick a position:
           ix <- randomRIO (0,size-1) :: IO Int
           pushL (arr ! ix) i
           when (i - id*producers < 10) $ printf " [%d] pushed %d \n" id i

   -- Each consumer doesn't quit until it has popped "perthread":
   sums <- forkJoin consumers $ \ id -> 
        let consume_loop sum  i | i == perthread = return sum
            consume_loop !sum i = do
              -- Randomly pick a position:
              ix <- randomRIO (0,size-1) :: IO Int
              -- Try to pop something, but not too hard:
              m <- spinPopN 100 (arr ! ix)
              case m of
                Just x -> do
                  when (i < 10) $ printf " [%d] popped #%d = %d\n" id i x
                  consume_loop (sum+x) (i+1)
                Nothing ->
                  consume_loop sum i
        in consume_loop 0 0

   printf "Reading sums from MVar...\n" 
   let finalSum = Prelude.sum sums
   putStrLn$ "Final sum: "++ show finalSum ++ ", per-consumer sums: "++show sums
   putStrLn$ "Checking that queue is finally null..."
   assertEqual "Correct final sum" (producers * expectedSum perthread) finalSum
   bs <- mapM nullQ qs
   if all id bs
     then putStrLn$ "Sum matched expected, test passed."
     else assertFailure "Queue was not empty!!"

-- Sum of first N numbers, starting with ZERO
expectedSum :: Integral a => a -> a
expectedSum n = (n * (n - 1)) `quot` 2


------------------------------------------------------------

-- | This creates an HUnit test list to perform all the tests above.
test_fifo :: DequeClass d => (forall elt. IO (d elt)) -> Test
test_fifo newq = TestList $ 
  [
    TestLabel "test_fifo_filldrain"  (TestCase$ assert $ newq >>= test_fifo_filldrain)
    -- Do half a million elements by default:
  , TestLabel "test_fifo_OneBottleneck_backoff" (TestCase$ assert $ newq >>= test_fifo_OneBottleneck True  numElems)
--  , TestLabel "test_fifo_OneBottleneck_aggressive" (TestCase$ assert $ newq >>= test_fifo_OneBottleneck False numElems)
--  , TestLabel "test the tests" (TestCase$ assert $ assertFailure "This SHOULD fail.")

  , TestLabel "test_contention_free_parallel" (TestCase$ assert $ test_contention_free_parallel True numElems newq)
  ] ++
  [ TestLabel ("test_random_array_comm_"++show size)
              (TestCase$ assert $ test_random_array_comm size numElems newq)
  | size <- [10,100]
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
-- Misc Helpers
----------------------------------------------------------------------------------------------------

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

forkJoin :: Int -> (Int -> IO b) -> IO [b]
forkJoin numthreads action = 
  do
     answers <- sequence (replicate numthreads newEmptyMVar) -- padding?
     forM_ (zip [0..] answers) $ \ (ix,mv) -> 
 	myfork "forkJoin worker" (action ix >>= putMVar mv)
     -- Reading answers:
     ls <- mapM readMVar answers
     return ls

spinPopBkoff :: DequeClass d => d t -> IO (t, Int)
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
spinPopHard :: (DequeClass d) => d t -> IO (t, Int)
spinPopHard q = loop 1
 where 
  loop n = do
     x <- tryPopR q 
     case x of 
       Nothing -> do loop (n+1)
       Just x  -> return (x, n)

spinPopN :: (DequeClass d) => Int -> d t -> IO (Maybe t)
spinPopN 0 q = return Nothing
spinPopN tries q = do
   x <- tryPopR q 
   case x of 
     Nothing      -> spinPopN (tries-1) q
     res@(Just _) -> return res

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i == end  = return ()
	   | otherwise = do fn i; loop (i+1)


----------------------------------------------------------------------------------------------------

