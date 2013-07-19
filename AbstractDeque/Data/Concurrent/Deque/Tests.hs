{-# LANGUAGE BangPatterns, RankNTypes, CPP, BangPatterns #-}

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
   test_parfib_work_stealing,

   -- * All deque tests, aggregated.
   tests_all,

   -- * Testing parameters
   numElems, getNumAgents, producerRatio,

   -- * Utility for tweaking test suites
   setTestThreads, appendLabels, appendLabel,

   -- * Test initialization, reading common configs
   stdTestHarness, Elt,

   -- * Misc helpers
   forkJoin, timeit, fibSize
 )
 where 

import Control.Monad
import           Data.Time.Clock
import           Data.Array as A
import           Data.IORef
import           Data.Int
import qualified Data.Set as S
import Text.Printf
import GHC.Conc (throwTo, threadDelay, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent (yield, forkOS, forkIO, ThreadId)
import Control.Exception (catch, SomeException, fromException, bracket, AsyncException(ThreadKilled))
import System.Environment (withArgs, getArgs, getEnvironment)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO, randomRIO)
import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit  (hUnitTestToTests)
import           Test.HUnit as HU

import Debug.Trace (trace)

import           Data.Concurrent.Deque.Class as C
import qualified Data.Concurrent.Deque.Reference as R


#if __GLASGOW_HASKELL__ >= 704
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
#else
import GHC.Conc (numCapabilities)
getNumCapabilities :: IO Int
getNumCapabilities = return numCapabilities

setNumCapabilities :: Int -> IO ()
setNumCapabilities = error "setNumCapabilities not supported in this older GHC!  Set NUMTHREADS and +RTS -N to match."

getNumProcessors :: IO Int
getNumProcessors = return 1 
#endif    

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
fibSize :: Int64
fibSize = case lookup "FIBSIZE" theEnv of
            Just s  -> read s
            Nothing -> 32

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
getNumAgents :: IO Int
getNumAgents = case lookup "NUMAGENTS" theEnv of 
                Nothing  -> do x <- getNumCapabilities
                               putStrLn$"Defaulting numAgents to numCapabilities: "++show x
                               return x
                Just str -> warnUsing ("NUMAGENTS = "++str) $ 
                            return (read str)

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


-- | Dig through the test constructors to find the leaf IO actions and bracket them
--   with a thread-setting action.
setTestThreads :: Int -> HU.Test -> HU.Test
setTestThreads nm tst = loop False tst
 where
   loop flg x = 
    case x of
      TestLabel lb t2 -> TestLabel (decor flg lb) (loop True t2)
      TestList ls -> TestList (map (loop flg) ls)
      TestCase io -> TestCase (bracketThreads nm io)

   -- We only need to insert the numcapabilities in the description string ONCE:
   decor False lb = "N"++show nm++"_"++ lb
   decor True  lb = lb

   bracketThreads :: Int -> IO a -> IO a
   bracketThreads n act =
     bracket (getNumCapabilities)
             setNumCapabilities
             (\_ -> do dbgPrint 1 ("\n   [Setting # capabilities to "++show n++" before test] \n")
                       setNumCapabilities n
                       act)

-- | Dig through the test constructors and add a new string to the first label found.
-- If no such label exists, add one.   
appendLabel :: String -> HU.Test -> HU.Test
appendLabel newLab tst = loop tst
 where
   loop x = 
    case x of
      TestLabel lb t2 -> TestLabel (newLab ++"_"++ lb) t2
      TestList ls     -> TestList (map loop ls)
      TestCase io     -> TestLabel newLab x

-- | This version has the option of being smarter about how it handles uniformly
-- labeling many tests.
appendLabels :: String -> [HU.Test] -> HU.Test
appendLabels newLab tst = TestList $ map (appendLabel newLab) tst

stdTestHarness :: (IO Test) -> IO ()
stdTestHarness genTests = do 
  putStrLn$ "Running with numElems "++show numElems
  putStrLn "Use NUMELEMS, NUMAGENTS, NUMTHREADS to control the size of this benchmark."
  args <- getArgs

  np <- getNumProcessors
  putStrLn $"Running on a machine with "++show np++" hardware threads."

  -- We allow the user to set this directly, because the "-t" based regexp selection
  -- of benchmarks is quite limited.
  let all_threads = case lookup "NUMTHREADS" theEnv of
                      Just str -> [read str]
                      Nothing -> S.toList$ S.fromList$
                        [1, 2, np `quot` 2, np, 2*np ]
  putStrLn $"Running tests for these thread settings: "  ++show all_threads
  all_tests <- genTests 

  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $ do 

    -- Hack, this shouldn't be necessary, but I'm having problems with -t:
    tests <- case all_threads of
              [one] -> do cap <- getNumCapabilities
                          unless (cap == one) $ setNumCapabilities one
                          return all_tests
              _ -> return$ TestList [ setTestThreads n all_tests | n <- all_threads ]
    TF.defaultMain$ hUnitTestToTests tests


----------------------------------------------------------------------------------------------------
-- Test a plain FIFO queue:
----------------------------------------------------------------------------------------------------

-- The type of data that we push over the queues.
type Elt = Int64

-- | This test serially fills up a queue and then drains it.
test_fifo_filldrain :: DequeClass d => d Elt -> IO ()
test_fifo_filldrain q = 
  do
     dbgPrintLn 1 "\nTest FIFO queue: sequential fill and then drain"
     dbgPrintLn 1 "==============================================="
--     let n = 1000
     let n = fromIntegral numElems
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
     let expected = sum [1..n] :: Elt
     dbgPrint 1 $ printf "\nSum of popped vals: %d should be %d\n" s expected
     when (s /= expected) (assertFailure "Incorrect sum!")
     return ()

{-# INLINABLE test_fifo_OneBottleneck #-}
-- | This test splits the 'numAgents' threads into producers and
-- consumers which all communicate through a SINGLE queue.  Each
-- thread performs its designated operation as fast as possible.  The
-- 'Int' argument 'total' designates how many total items should be
-- communicated (irrespective of 'numAgents').
test_fifo_OneBottleneck :: DequeClass d => Bool -> Int -> d Elt -> IO ()
test_fifo_OneBottleneck doBackoff total q = 
  do
     assertBool "test_fifo_OneBottleneck requires thread safe left end"  (leftThreadSafe q)
     assertBool "test_fifo_OneBottleneck requires thread safe right end" (rightThreadSafe q)
    
     dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers thru 1 queue"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     dbgPrintLn 1 "======================================================"       

     bl <- nullQ q
     dbgPrintLn 1$ "Check that queue is initially null: "++show bl

     numAgents <- getNumAgents      
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = max 1 (numAgents - producers)
	 perthread  = total `quot` producers
         (perthread2,remain) = total `quotRem` consumers

     numCap <- getNumCapabilities     
     when (not doBackoff && (numCap == 1 || numCap < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     dbgPrint 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
     dbgPrint 1 $ printf "Forking %d consumer threads, each consuming ~%d elements.\n" consumers perthread2
    
     forM_ [0..producers-1] $ \ ind -> 
 	myfork "producer thread" $ do
          let start = ind * perthread 
          dbgPrint 1 $ printf "  * Producer thread %d pushing ints from %d to %d \n" ind start (start+perthread - 1)
          forI_ start (start+perthread) $ \ i -> do 
	     pushL q i
             -- when (i < start + 10) $ dbgPrint 1 $ printf " [p%d] pushed %d \n" ind i

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
     assertEqual "Correct final sum" (expectedSum (fromIntegral$ producers * perthread)) finalSum
     dbgPrintLn 1$ "Checking that queue is finally null..."
     b <- nullQ q
     if b then dbgPrintLn 1$ "Sum matched expected, test passed."
          else assertFailure "Queue was not empty!!"

-- | This test uses a separate queue per producer/consumer pair.  The queues
-- are used in a single-writer single-reader.
test_contention_free_parallel :: DequeClass d => Bool -> Int -> IO (d Elt) -> IO ()
test_contention_free_parallel doBackoff total newqueue = 
  do 
     dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers thru N queues"
               ++(if doBackoff then " (with backoff)" else "(hard busy wait)")
     dbgPrintLn 1 "======================================================"       
     mv <- newEmptyMVar

     numAgents <- getNumAgents
     let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
	 consumers = producers -- Must be matched
	 perthread  = total `quot` producers
     qs <- sequence (replicate consumers newqueue)

     numCap <- getNumCapabilities 
     when (not doBackoff && (numCap == 1 || numCap < producers + consumers)) $ 
       error$ "The aggressively busy-waiting version of the test can only run with the right thread settings."
     
     dbgPrint 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
     dbgPrint 1 $ printf "Forking %d consumer threads, each consuming %d elements.\n" consumers perthread
    
     forM_ (zip [0..producers-1] qs) $ \ (id, q) -> 
 	myfork "producer thread" $
          let start = id*perthread in
          forI_ 0 perthread $ \ i -> do 
	     pushL q i
             -- when (i - id*producers < 10) $ dbgPrint 1 $ printf " [%d] pushed %d \n" id i

     forM_ (zip [0..consumers-1] qs) $ \ (id, q) -> 
 	myfork "consumer thread" $ do 
          let consume_loop sum maxiters i | i == perthread = return (sum, maxiters)
              consume_loop !sum !maxiters i = do
                (x,iters) <- if doBackoff then spinPopBkoff q 
                                          else spinPopHard  q
                when (i < 10) $ dbgPrint 1 $ printf " [%d] popped #%d = %d \n" id i x
                unless (x == fromIntegral i) $
                  error $ "Message out of order! Expected "++show i++" recevied "++show x
                consume_loop (sum+x) (max maxiters iters) (i+1)
          pr <- consume_loop 0 0 0
	  putMVar mv pr

     dbgPrint 1 $ printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum (map fst ls)
     dbgPrintLn 1$ "Consumers DONE.  Maximum retries for each consumer thread: "++ show (map snd ls)
     dbgPrintLn 1$ "All messages received in order.  Final sum: "++ show finalSum
     assertEqual "Correct final sum" (fromIntegral producers * expectedSum (fromIntegral perthread)) finalSum          
     dbgPrintLn 1$ "Checking that queue is finally null..."
     bs <- mapM nullQ qs
     if all id bs
       then dbgPrintLn 1$ "Sum matched expected, test passed."
       else assertFailure "Queue was not empty!!"


{-# INLINE test_random_array_comm #-}
-- | This test uses a number of producer and consumer threads which push and pop
-- elements from random positions in an array of FIFOs.
test_random_array_comm :: DequeClass d => Int -> Int -> IO (d Elt) -> IO ()
test_random_array_comm size total newqueue = do
   assertBool "positive size" (size > 0)
  
   qs <- sequence (replicate size newqueue)
--   arr <- V.thaw $ V.fromlist qs
   let arr = A.listArray (0,size-1) qs

   assertBool "test_random_array_comm requires thread safe left end"  (leftThreadSafe (head qs))
   assertBool "test_random_array_comm requires thread safe right end" (rightThreadSafe (head qs))
   
   dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers select random queues"
   dbgPrintLn 1 "======================================================"       

   numAgents <- getNumAgents
   let producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
       consumers = max 1 (numAgents - producers)
       perthread           = fromIntegral (total `quot` producers)
       (perthread2,remain) = total `quotRem` consumers
   
   dbgPrintLn 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
   dbgPrintLn 1 $ printf "Forking %d consumer threads, each consuming ~%d elements.\n" consumers perthread2

   forM_ [0..producers-1] $ \ ind -> 
      myfork "producer thread" $
        for_ 0 perthread $ \ i -> do
           -- Randomly pick a position:
           ix <- randomRIO (0,size - 1) :: IO Int
           pushL (arr ! ix) i
           -- when (i - ind*producers < 10) $ dbgPrint 1 $ printf " [%d] pushed %d \n" ind i

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
   assertEqual "Correct final sum" (fromIntegral producers * expectedSum perthread) finalSum
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
tests_fifo newq = appendLabels "single-ended-queue-tests" $ 
  tests_basic newq ++ 
  tests_fifo_exclusive newq
  
{-# INLINE tests_fifo_exclusive #-}
-- | Tests exclusive to single-ended (threadsafe) queues:
tests_fifo_exclusive :: DequeClass d => (forall elt. IO (d elt)) -> [Test]
tests_fifo_exclusive newq = 
  [ TestLabel "test_fifo_OneBottleneck_backoff" (TestCase$ assertT $ newq >>= test_fifo_OneBottleneck True  numElems)
--  , TestLabel "test_fifo_OneBottleneck_aggressive" (TestCase$ assert $ newq >>= test_fifo_OneBottleneck False numElems)
--  , TestLabel "test the tests" (TestCase$ assert $ assertFailure "This SHOULD fail.")
  ] ++
  [ TestLabel ("test_random_array_comm_"++show size)
              (TestCase$ assertT $ test_random_array_comm size numElems newq)
  | size <- [10,100]
  ]

{-# INLINE tests_basic #-}
-- | Tests that don't require thread safety at either end.
tests_basic :: DequeClass d => (forall elt. IO (d elt)) -> [Test]
tests_basic newq =
  [ TestLabel "test_fifo_filldrain"           (TestCase$ assertT $ newq >>= test_fifo_filldrain)
  , TestLabel "test_contention_free_parallel" (TestCase$ assertT $ test_contention_free_parallel True numElems newq)
  ]

assertT :: IO () -> IO ()
assertT = timeit . assert

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


{-# INLINE test_random_work_stealing #-}
-- | This test uses a number of worker threads which randomly either push or pop work
-- to their local work stealing deque.  Also, there are consumer (always thief)
-- threads which never produce and only consume.
test_random_work_stealing :: (DequeClass d, PopL d) => Int -> IO (d Elt) -> IO ()
test_random_work_stealing total newqueue = do
   
   dbgPrintLn 1$ "\nTest FIFO queue: producers & consumers select random queues"
   dbgPrintLn 1 "======================================================"       

   numAgents <- getNumAgents
   let producers, consumers :: Int 
       perthread, realtotal, perthread2 :: Elt
       producers = max 1 (round$ producerRatio * (fromIntegral numAgents) / (producerRatio + 1))
       consumers = max 1 (fromIntegral numAgents - producers)
       perthread = fromIntegral total `quot` fromIntegral producers
       realtotal = perthread * fromIntegral producers
       (perthread2,remain) = realtotal `quotRem` fromIntegral consumers
       
   qs <- sequence (replicate producers newqueue)   
   -- A work-stealing deque only has ONE threadsafe end:
   assertBool "test_random_array_comm requires thread safe right end" (rightThreadSafe (head qs))
   let arr = A.listArray (0,producers - 1) qs   

   dbgPrint 1 $ printf "Forking %d producer threads, each producing %d elements.\n" producers perthread
   dbgPrint 1 $ printf "Forking %d consumer threads, each consuming ~%d elements.\n" consumers perthread2
   
   prod_results <- newEmptyMVar
   forM_ (zip [0..producers-1] qs) $ \ (ind,myQ) -> do 
      myfork "producer thread" $
        let loop :: Elt -> Elt -> Elt -> IO ()
            loop i !pops !acc 
              | i == perthread = putMVar prod_results (pops,acc)
              | otherwise = do 
                 -- Randomly push or pop:
                 b   <-  randomIO  :: IO Bool
                 if b then do
                   x <- spinPopN 100 (tryPopL myQ)
                   -- In the case where we pop, we do NOT advance 'i', saving that
                   -- for the next iteration that acutally does a push.
                   case x of
                     Nothing -> loop i  pops    acc
                     Just n  -> loop i (pops+1) (n+acc)
                   else do
                   pushL myQ i
                   -- when (i - ind*producers < 10) $ dbgPrint 1 $ printf " [%d] pushed %d \n" ind i
                   loop (i+1) pops acc
        in loop 0 0 0

   -- In this version
   consumer_sums <- forkJoin consumers $ \ ind ->
        let mymax = if ind==0 then perthread2 + remain else perthread2             
            consume_loop !summ !successes i 
               | i == mymax = return (successes, summ)
               | otherwise  = do 
              -- Randomly pick a position:
              ix <- randomRIO (0,producers - 1) :: IO Int
              -- Try to pop something, but not too hard:
              m <- spinPopN 100 (tryPopR (arr ! ix))
              case m of
                Just x -> do
                  when (i < 10) $ dbgPrint 1 $ printf " [%d] popped try#%d = %d\n" ind i x
                  consume_loop (summ+x) (successes+1) (i+1)
                Nothing ->
                  consume_loop summ successes (i+1) -- Increment even if we don't get a result.
        in do dbgPrintLn 1 ("   Beginning consumer thread loop for "++show mymax ++" attempts.")
              consume_loop 0 0 0
   -- <-- Consumers are finished as of here.

   dbgPrintLn 1  "Reading sums from MVar..."
   prod_ls <- mapM (\_ -> takeMVar prod_results) [1..producers]

   -- We sequentially read out the leftovers after all the parallel tasks are done:
   leftovers <- forM qs $ \ q ->
     let loop !cnt !acc = do
           x <- tryPopR q -- This should work as a popR OR a popL.
           case x of
             Nothing -> return (cnt,acc)
             Just n  -> loop (cnt+1) (acc+n)
     in loop 0 0
        
   let finalSum = Prelude.sum (map snd consumer_sums ++ 
                               map snd prod_ls ++ 
                               map snd leftovers)
   dbgPrintLn 0$ "Final sum: "++ show finalSum ++ ", producer/consumer/leftover sums: "++show (prod_ls, consumer_sums, leftovers)
   dbgPrintLn 1$ "Total pop events: "++ show (Prelude.sum (map fst consumer_sums ++ 
                                                           map fst prod_ls ++ 
                                                           map fst leftovers))
                 ++" should be "++ (show$ realtotal)
   dbgPrintLn 1$ "Checking that queue is finally null..."
   assertEqual "Correct final sum" (fromIntegral producers * expectedSum perthread) finalSum
   bs <- mapM nullQ qs
   if all id bs
     then dbgPrintLn 1$ "Sum matched expected, test passed."
     else assertFailure "Queue was not empty!!"


{-# INLINE test_parfib_work_stealing #-}
test_parfib_work_stealing :: (DequeClass d, PopL d) => Elt -> IO (d Elt) -> IO ()
test_parfib_work_stealing origInput newqueue = do
  putStrLn$ " [parfib] Computing fib("++show origInput++")"
  numAgents <- getNumAgents
  qs <- sequence (replicate numAgents newqueue)
  let arr = A.listArray (0,numAgents - 1) qs 
  
  let parfib !myId !myQ !mySum !num
        | num <= 2  =
          do x <- tryPopL myQ
             case x of
               Nothing -> trySteal myId myQ (mySum+1)
               Just n  -> parfib myId myQ   (mySum+1) n
        | otherwise = do 
          pushL       myQ       (num-1)
          parfib myId myQ mySum (num-2)
          
      trySteal !myId !myQ !mySum =
        let loop ind
              -- After we finish one sweep... we're completely done.
              | ind == myId     = return mySum
              | ind == size arr = loop 0
              | otherwise = do
                  x <- tryPopR (arr ! ind)
                  case x of
                    Just n  -> parfib myId myQ mySum n
                    Nothing -> do yield
                                  loop (ind+1)
        in loop (myId+1)

      size a = let (st,en) = A.bounds a in en - st + 1 
  
  partial_sums <- forkJoin numAgents $ \ myId ->
    if myId == 0
    then parfib   myId (arr ! myId) 0 origInput
    else trySteal myId (arr ! myId) 0 

  putStrLn$ " [parfib] fib("++show origInput++") = "++show (sum partial_sums)


{-# INLINE tests_wsqueue #-}
-- | Aggregate tests for work stealing queues.  None of these require thread-safety
-- on the left end.  There is some duplication with tests_fifo.
tests_wsqueue :: (PopL d) => (forall elt. IO (d elt)) -> Test
tests_wsqueue newq = appendLabels "work-stealing-deque-tests"$ 
 tests_wsqueue_exclusive newq ++
 tests_basic newq 

{-# INLINE tests_wsqueue_exclusive #-}
-- Internal: factoring this out.
tests_wsqueue_exclusive :: (PopL d) => (forall elt. IO (d elt)) -> [Test]
tests_wsqueue_exclusive newq = 
 [ TestLabel "test_ws_triv1"             (TestCase$ assertT $ newq >>= test_ws_triv1)
 , TestLabel "test_ws_triv2"             (TestCase$ assertT $ newq >>= test_ws_triv2)
 , TestLabel "test_random_work_stealing" (TestCase$ assertT $ test_random_work_stealing numElems newq)
 , TestLabel "test_parfib_generic"       (TestCase$ assertT $ test_parfib_work_stealing fibSize newq)
 ]

----------------------------------------------------------------------------------------------------
-- Combine all tests -- for a deques supporting all capabilities.
----------------------------------------------------------------------------------------------------

-- | This requires double ended queues that are threadsafe on BOTH ends.
tests_all :: (PopL d) => (forall elt. IO (d elt)) -> Test
tests_all newq = appendLabels "full-deque-tests" $  
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

-- | Pop from the right end more gently.
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
		      then do dbgPrint 1 "!"
                              threadDelay n -- 1ms after 1K fails, 2 after 2K...
		      else when (n > hardspinfor) $ do 
                             dbgPrint 1 "."
                             hFlush stdout
			     yield -- At LEAST yield... you'd think this is pretty strong backoff.
		     loop (n+1)
       Just x  -> return (x, n)


-- | This is always a crazy and dangerous thing to do -- GHC cannot deschedule this
-- spinning thread because it does not allocate.  Thus one can run into a deadlock
-- situation where the consumer holds the capabilitity hostage, not letting the
-- producer run (and therefore unblock it).
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
for_ :: Monad m => Elt -> Elt -> (Elt -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i == end  = return ()
	   | otherwise = do fn i; loop (i+1)

forI_ :: Monad m => Int -> Int -> (Elt -> m ()) -> m ()
forI_ st en = for_ (fromIntegral st) (fromIntegral en)

----------------------------------------------------------------------------------------------------
-- DEBUGGING
----------------------------------------------------------------------------------------------------

-- | Debugging flag shared by several modules.
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

timeit :: IO a -> IO a 
timeit ioact = do 
   start <- getCurrentTime
   res <- ioact
   end   <- getCurrentTime
   putStrLn$ "SELFTIMED: " ++ show (diffUTCTime end start)
   return res
