{-# LANGUAGE BangPatterns, RankNTypes #-}

-- | This module contains a battery of simple tests for queues
--   implementing the interface defined in
-- ` Data.Concurrent.Deque.Class`.

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Data.Concurrent.Deque.Tests 
 ( 
   -- * Tests for simple FIFOs.
   test_fifo_filldrain, test_fifo_OneBottleneck, tests_fifo,

   -- * Tests for Work-stealing queues.
   test_ws_triv1, test_ws_triv2, tests_wsqueue,

   -- * All deque tests, aggregated.
   tests_all,

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
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO, randomRIO)
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
     dbgPrintLn 1 "\nTest FIFO queue: sequential fill and then drain"
     dbgPrintLn 1 "==============================================="
--     let n = 1000
     let n = numElems
     dbgPrintLn 1$ "Done creating queue.  Pushing "++show n++" elements:"
     forM_ [1..n] $ \i -> do 
       pushL q i
       when (i < 200) $ dbgPrint 1 $ printf " %d" i
     dbgPrintLn 1 "\nDone filling queue with elements.  Now popping..."
     
     let loop 0 !sumR = return sumR
         loop i !sumR = do 
           (x,_) <- spinPopBkoff q 
           when (i < 200) $ dbgPrint 1 $ printf " %d" x
           loop (i-1) (sumR + x)
     s <- loop n 0
     let expected = sum [1..n] :: Int
     dbgPrint 1 $ printf "\nSum of popped vals: %d should be %d\n" s expected
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
    
     dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers thru 1 queue"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     dbgPrintLn 1 "======================================================"       

     bl <- nullQ q
     dbgPrintLn 1$ "Check that queue is initially null: "++show bl
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = max 1 (numAgents - producers)
	 perthread  = total `quot` producers
         (perthread2,remain) = total `quotRem` consumers
     
     when (not doBackoff && (numCapabilities == 1 || numCapabilities < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     dbgPrint 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
     dbgPrint 1 $ printf "Forking %d consumer threads, each consuming ~%d elements.\n" consumers perthread2
    
     forM_ [0..producers-1] $ \ ind -> 
 	myfork "producer thread" $ do
          let start = ind * perthread 
          dbgPrint 1 $ printf "  * Producer thread %d pushing ints from %d to %d \n" ind start (start+perthread - 1)
          for_ start (start+perthread) $ \ i -> do 
	     pushL q i
             when (i < start + 10) $ dbgPrint 1 $ printf " [p%d] pushed %d \n" ind i

     ls <- forkJoin consumers $ \ ind ->         
          let mymax = if ind==0 then perthread2 + remain else perthread2
              consume_loop summ maxiters i | i == mymax = return (summ, maxiters)
              consume_loop !summ !maxiters i = do
                (x,iters) <- if doBackoff then spinPopBkoff q 
                                          else spinPopHard  q
                when (i >= mymax - 10) $ dbgPrint 1 $ printf " [c%d] popped #%d = %d \n" ind i x
                consume_loop (summ+x) (max maxiters iters) (i+1)
          in consume_loop 0 0 0

     let finalSum = Prelude.sum (map fst ls)
     dbgPrintLn 1$ "Consumers DONE.  Maximum retries for each consumer thread: "++ show (map snd ls)
     dbgPrintLn 1$ "Final sum: "++ show finalSum
     assertEqual "Correct final sum" (expectedSum (producers * perthread)) finalSum
     dbgPrintLn 1$ "Checking that queue is finally null..."
     b <- nullQ q
     if b then dbgPrintLn 1$ "Sum matched expected, test passed."
          else assertFailure "Queue was not empty!!"

-- | This test uses a separate queue per consumer thread.  The queues
-- are used in a single-writer multiple-reader fashion (mailboxes).
test_contention_free_parallel :: DequeClass d => Bool -> Int -> IO (d Int) -> IO ()
test_contention_free_parallel doBackoff total newqueue = 
  do 
     dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers thru N queues"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     dbgPrintLn 1 "======================================================"       
     mv <- newEmptyMVar
          
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = producers -- Must be matched
	 perthread  = total `quot` producers
     qs <- sequence (replicate consumers newqueue)
     
     when (not doBackoff && (numCapabilities == 1 || numCapabilities < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     dbgPrint 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
     dbgPrint 1 $ printf "Forking %d consumer threads, each consuming %d elements.\n" consumers perthread
    
     forM_ (zip [0..producers-1] qs) $ \ (id, q) -> 
 	myfork "producer thread" $
          let start = id*perthread in
          for_ 0 perthread $ \ i -> do 
	     pushL q i
             when (i - id*producers < 10) $ dbgPrint 1 $ printf " [%d] pushed %d \n" id i

     forM_ (zip [0..consumers-1] qs) $ \ (id, q) -> 
 	myfork "consumer thread" $ do 
          let consume_loop sum maxiters i | i == perthread = return (sum, maxiters)
              consume_loop !sum !maxiters i = do
                (x,iters) <- if doBackoff then spinPopBkoff q 
                                          else spinPopHard  q
                when (i < 10) $ dbgPrint 1 $ printf " [%d] popped #%d = %d \n" id i x
                unless (x == i) $ error $ "Message out of order! Expected "++show i++" recevied "++show x
                consume_loop (sum+x) (max maxiters iters) (i+1)
          pr <- consume_loop 0 0 0
	  putMVar mv pr

     dbgPrint 1 $ printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum (map fst ls)
     dbgPrintLn 1$ "Consumers DONE.  Maximum retries for each consumer thread: "++ show (map snd ls)
     dbgPrintLn 1$ "All messages received in order.  Final sum: "++ show finalSum
     assertEqual "Correct final sum" (producers * expectedSum perthread) finalSum          
     dbgPrintLn 1$ "Checking that queue is finally null..."
     bs <- mapM nullQ qs
     if all id bs
       then dbgPrintLn 1$ "Sum matched expected, test passed."
       else assertFailure "Queue was not empty!!"



-- | This test uses a number of producer and consumer threads which push and pop
-- elements from random positions in an array of FIFOs.
test_random_array_comm :: DequeClass d => Int -> Int -> IO (d Int) -> IO ()
test_random_array_comm size total newqueue = do
   assertBool "positive size" (size > 0)
  
   qs <- sequence (replicate size newqueue)
--   arr <- V.thaw $ V.fromlist qs
   let arr = A.listArray (0,size-1) qs

   assertBool "test_random_array_comm requires thread safe left end"  (leftThreadSafe (head qs))
   assertBool "test_random_array_comm requires thread safe right end" (rightThreadSafe (head qs))
   
   dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers select random queues"
   dbgPrintLn 1 "======================================================"       

   let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
       consumers = max 1 (numAgents - producers)
       perthread  = total `quot` producers
       (perthread2,remain) = total `quotRem` consumers
   
   dbgPrintLn 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
   dbgPrintLn 1 $ printf "Forking %d consumer threads, each consuming ~%d elements.\n" consumers perthread2

   forM_ [0..producers-1] $ \ ind -> 
      myfork "producer thread" $
        for_ 0 perthread $ \ i -> do
           -- Randomly pick a position:
           ix <- randomRIO (0,size-1) :: IO Int
           pushL (arr ! ix) i
           when (i - ind*producers < 10) $ dbgPrint 1 $ printf " [%d] pushed %d \n" ind i

   -- Each consumer doesn't quit until it has popped "perthread":
   sums <- forkJoin consumers $ \ ind ->
        let mymax = if ind==0 then perthread2 + remain else perthread2
            consume_loop summ  i | i == mymax = return summ
            consume_loop !summ i = do
              -- Randomly pick a position:
              ix <- randomRIO (0,size-1) :: IO Int
              -- Try to pop something, but not too hard:
              m <- spinPopN 100 (tryPopR (arr ! ix))
              case m of
                Just x -> do
                  when (i < 10) $ dbgPrint 1 $ printf " [%d] popped #%d = %d\n" ind i x
                  consume_loop (summ+x) (i+1)
                Nothing ->
                  consume_loop summ i
        in consume_loop 0 0

   dbgPrintLn 1 "Reading sums from MVar..." 
   let finalSum = Prelude.sum sums
   dbgPrintLn 1$ "Final sum: "++ show finalSum ++ ", per-consumer sums: "++show sums
   dbgPrintLn 1$ "Checking that queue is finally null..."
   assertEqual "Correct final sum" (producers * expectedSum perthread) finalSum
   bs <- mapM nullQ qs
   if all id bs
     then dbgPrintLn 1$ "Sum matched expected, test passed."
     else assertFailure "Queue was not empty!!"

-- Sum of first N numbers, starting with ZERO
expectedSum :: Integral a => a -> a
expectedSum n = (n * (n - 1)) `quot` 2


------------------------------------------------------------

-- | This creates an HUnit test list to perform all the tests that apply to a
--   single-ended (threadsafe) queue.  It requires thread safety at /both/ ends.
tests_fifo :: DequeClass d => (forall elt. IO (d elt)) -> Test
tests_fifo newq = TestLabel "single-ended-queue-tests"$ TestList $
  tests_basic newq ++ 
  tests_fifo_exclusive newq
  
-- | Tests exclusive to single-ended (threadsafe) queues:
tests_fifo_exclusive :: DequeClass d => (forall elt. IO (d elt)) -> [Test]
tests_fifo_exclusive newq = 
  [ TestLabel "test_fifo_OneBottleneck_backoff" (TestCase$ assert $ newq >>= test_fifo_OneBottleneck True  numElems)
--  , TestLabel "test_fifo_OneBottleneck_aggressive" (TestCase$ assert $ newq >>= test_fifo_OneBottleneck False numElems)
--  , TestLabel "test the tests" (TestCase$ assert $ assertFailure "This SHOULD fail.")
  ] ++
  [ TestLabel ("test_random_array_comm_"++show size)
              (TestCase$ assert $ test_random_array_comm size numElems newq)
  | size <- [10,100]
  ]


-- | Tests that don't require thread safety at either end.
tests_basic :: DequeClass d => (forall elt. IO (d elt)) -> [Test]
tests_basic newq =
  [ TestLabel "test_fifo_filldrain"  (TestCase$ assert $ newq >>= test_fifo_filldrain)
  , TestLabel "test_contention_free_parallel" (TestCase$ assert $ test_contention_free_parallel True numElems newq)    
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


-- | This test uses a number of worker threads which randomly either push or pop work
-- to their local work stealing deque.  Also, there are consumer (always thief)
-- threads which never produce and only consume.
test_random_work_stealing :: (DequeClass d, PopL d) => Int -> IO (d Int) -> IO ()
test_random_work_stealing total newqueue = do
   
   dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers select random queues"
   dbgPrintLn 1 "======================================================"       

   let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
       consumers = max 1 (numAgents - producers)
       perthread  = total `quot` producers
       (perthread2,remain) = total `quotRem` consumers
       
   qs <- sequence (replicate producers newqueue)   
   -- A work-stealing deque only has ONE threadsafe end:
   assertBool "test_random_array_comm requires thread safe right end" (rightThreadSafe (head qs))
   let arr = A.listArray (0,producers - 1) qs   

   dbgPrint 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
   dbgPrint 1 $ printf "Forking %d consumer threads, each consuming %d elements.\n" consumers perthread2
   
   prod_results <- newEmptyMVar
   forM_ (zip [0..producers-1] qs) $ \ (ind,myQ) -> do 
      myfork "producer thread" $
        let loop i !acc | i == perthread = putMVar prod_results acc
            loop i !acc = 
              do -- Randomly push or pop:
                 b   <-  randomIO  :: IO Bool
                 if b then do
                   x <- spinPopN 100 (tryPopL myQ)
                   case x of
                     Nothing -> loop i acc
                     Just n  -> loop i (n+acc)
                   else do
                   pushL myQ i
                   when (i - ind*producers < 10) $ dbgPrint 1 $ printf " [%d] pushed %d \n" ind i
                   loop (i+1) acc
        in loop 0 0

   consumer_sums <- forkJoin consumers $ \ ind ->
        let mymax = if ind==0 then perthread2 + remain else perthread2     
            consume_loop summ  i | i == mymax = return summ
            consume_loop !summ i = do
              -- Randomly pick a position:
              ix <- randomRIO (0,producers - 1) :: IO Int
              -- Try to pop something, but not too hard:
              m <- spinPopN 100 (tryPopR (arr ! ix))
              case m of
                Just x -> do
                  when (i < 10) $ dbgPrint 1 $ printf " [%d] popped try#%d = %d\n" ind i x
                  consume_loop (summ+x) (i+1)
                Nothing ->
                  consume_loop summ (i+1) -- Increment even if we don't get a result.
        in consume_loop 0 0
   -- Consumers are finished as of here.

   dbgPrintLn 1  "Reading sums from MVar..."
   prod_ls <- mapM (\_ -> takeMVar prod_results) [1..producers]

   -- We sequentially read out the leftovers after all the parallel tasks are done:
   leftovers <- forM qs $ \ q ->
     let loop !acc = do
           x <- tryPopR q -- This should work as a popR OR a popL.
           case x of
             Nothing -> return acc
             Just n  -> loop (acc+n)
     in loop 0
        
   let finalSum = Prelude.sum (consumer_sums ++ prod_ls ++ leftovers)
   dbgPrintLn 1$ "Final sum: "++ show finalSum ++ ", producer/consumer/leftover sums: "++show (prod_ls, consumer_sums, leftovers)
   dbgPrintLn 1$ "Checking that queue is finally null..."
   assertEqual "Correct final sum" (producers * expectedSum perthread) finalSum
   bs <- mapM nullQ qs
   if all id bs
     then dbgPrintLn 1$ "Sum matched expected, test passed."
     else assertFailure "Queue was not empty!!"



-- | Aggregate tests for work stealing queues.  None of these require thread-safety
-- on the left end.  There is some duplication with tests_fifo.
tests_wsqueue :: (PopL d) => (forall elt. IO (d elt)) -> Test
tests_wsqueue newq = TestLabel "work-stealing-deque-tests"$ TestList $
 tests_basic newq ++
 tests_wsqueue_exclusive newq

-- Internal: factoring this out.
tests_wsqueue_exclusive :: (PopL d) => (forall elt. IO (d elt)) -> [Test]
tests_wsqueue_exclusive newq = 
 [ TestLabel "test_ws_triv1"  (TestCase$ assert $ newq >>= test_ws_triv1)
 , TestLabel "test_ws_triv2"  (TestCase$ assert $ newq >>= test_ws_triv2)
 , TestLabel "test_random_work_stealing" (TestCase$ assert $ test_random_work_stealing numElems newq)
 ]

----------------------------------------------------------------------------------------------------
-- Combine all tests -- for a deques supporting all capabilities.
----------------------------------------------------------------------------------------------------

-- | This requires double ended queues that are threadsafe on BOTH ends.
tests_all :: (PopL d) => (forall elt. IO (d elt)) -> Test
tests_all newq = TestLabel "full-deque-tests"$ TestList $ 
  tests_basic newq ++
  tests_fifo_exclusive newq ++
  tests_wsqueue_exclusive newq 

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
	  (dbgPrintLn 0$ "Warning: Failed to pop "++ show warnafter ++ 
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
                             hFlush stdout
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

spinPopN :: Int -> IO (Maybe t) -> IO (Maybe t)
spinPopN 0 _ = return Nothing
spinPopN tries act = do
   x <- act
   case x of 
     Nothing      -> spinPopN (tries-1) act
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
-- DEBUGGING
----------------------------------------------------------------------------------------------------

-- | Debugging flag shared by all accelerate-backend-kit modules.
--   This is activated by setting the environment variable DEBUG=1..5
dbg :: Int
dbg = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         trace (" ! Responding to env Var: DEBUG="++s)$
         case reads s of
           ((n,_):_) -> n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

defaultDbg :: Int
defaultDbg = 0

-- | Print if the debug level is at or above a threshold.
dbgPrint :: Int -> String -> IO ()
dbgPrint lvl str = if dbg < lvl then return () else do
--    hPutStrLn stderr str
    -- hPrintf stderr str 
    -- hFlush stderr
    printf str
    hFlush stdout

dbgPrintLn :: Int -> String -> IO ()
dbgPrintLn lvl str = dbgPrint lvl (str++"\n")
