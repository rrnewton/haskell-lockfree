{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Int
import Data.List
import Data.List.Split  hiding (split)
import GHC.Exts (traceEvent)
--import Benchmarks.BinSearch
import System.Random
import System.CPUTime  (getCPUTime)
import System.CPUTime.Rdtsc
import Text.Printf
import GHC.Conc (forkIO, numCapabilities, threadDelay, killThread)
import Control.Concurrent.MVar

----------------------------------------------------------------------------------------------------
--    Main Script
----------------------------------------------------------------------------------------------------

main = do 
  putStrLn "Testing how many traceEvents we can do in a second."
--  binSearch 3 (0.99, 1.01) testTrace
  freq <- measureFreq

  putStrLn "\nFirst doing a test on one thread:"
  timeAndKillThreads 1 freq "IORef incrs" (return ())
  timeAndKillThreads 1 freq "traceEvent" (traceEvent "ConstString")

  putStrLn$ "\nSecond, doing a test on all "++show numCapabilities++" threads at once "
  timeAndKillThreads numCapabilities freq "IORef incrs" (return ())
  timeAndKillThreads numCapabilities freq "traceEvent" (traceEvent "ConstString")

  putStrLn "Done Testing."


--------------------------------------------------------------------------------
-- Helper and timing routines
--------------------------------------------------------------------------------

timeAndKillThreads :: Int -> Int64 -> String -> IO () -> IO ()
timeAndKillThreads numthreads freq msg action =
  do 

-- Note, if the IORefs are allocated on the main thread the throughput
-- in the parallel case plummets.  Presumably this is due to false
-- sharing as they get bump-allocated.
#if 0
     counters <- forM [1..numthreads] (const$ newIORef (1::Int64)) 
     tids <- forM counters $ \counter -> 
 	       forkIO $ infloop counter
#else
     mv <- newEmptyMVar
     tids <- forM [1..numthreads] $ \_ -> 
 	        forkIO $ do r <- newIORef (1::Int64)
			    putMVar mv r 
			    infloop r 
     counters <- forM [1..numthreads] (const$ takeMVar mv)
#endif

     threadDelay (1000*1000) -- One second
     mapM_ killThread tids

     finals <- mapM readIORef counters
--     printf "Across %d threads got these throughputs: %s\n" numthreads (show finals)
     let mean :: Double = fromIntegral (foldl1 (+) finals) / fromIntegral numthreads
         cycles_per :: Double = fromIntegral freq / mean

     printResult (round mean :: Int64) msg cycles_per

 where 
   infloop !counter = 
     do action
	incr counter        
	infloop counter 

   incr !counter = 
     do -- modifyIORef counter (+1) -- Not strict enough!
	c <- readIORef counter
	let c' = c+1
	_ <- evaluate c'
	writeIORef counter c'     

printResult ::  Int64 -> String -> Double -> IO ()
printResult total msg cycles_per = 
     putStrLn$ "    "++ padleft 11 (commaint total) ++" per/second average  "++ padright 27 ("["++msg++"]") ++" ~ "
	       ++ fmt_num cycles_per ++" cycles"


-- Readable large integer printing:
commaint :: Integral a => a -> String
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)

padleft :: Int -> String -> String
padleft n str | length str >= n = str
padleft n str | otherwise       = take (n - length str) (repeat ' ') ++ str

padright :: Int -> String -> String
padright n str | length str >= n = str
padright n str | otherwise       = str ++ take (n - length str) (repeat ' ')


fmt_num :: (RealFrac a, PrintfArg a) => a -> String
fmt_num n = if n < 100 
	    then printf "%.2f" n
	    else commaint (round n :: Integer)

-- Measure clock frequency, spinning rather than sleeping to try to
-- stay on the same core.
measureFreq :: IO Int64
measureFreq = do 
  let second = 1000 * 1000 * 1000 * 1000 -- picoseconds are annoying
  t1 <- rdtsc 
  start <- getCPUTime
  let loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
	  if (cput - start < second) 
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint (n::Int64)
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (t2 - t1)

----------------------------------------------------------------------------------------------------
{-
  Results:

  Sandybridge macbook air, first WITHOUT -threaded:

     Approx getCPUTime calls per second: 324,905
   First doing a test on one thread:
     43,804,752 per/second average  [IORef incrs]               ~ 41.53 cycles
        461,589 per/second average  [traceEvent]                ~ 3,941 cycles
 
  Westmere 3.1 ghz 4core noHT, first WITHOUT -threaded:

     Approx getCPUTime calls per second: 558,696
     First doing a test on one thread:
	86,823,292 per/second average  [IORef incrs]               ~ 35.63 cycles
	   215,086 per/second average  [traceEvent]                ~ 14,383 cycles

  Next, both WITH -threaded:
  Sandybridge macbook air, -N2:
    First doing a test on one thread:
	 57,743,676 per/second average  [IORef incrs]               ~ 30.34 cycles
	    425,853 per/second average  [traceEvent]                ~ 4,114 cycles
    Second, doing a test on all 2 threads at once 
	 50,622,096 per/second average  [IORef incrs]               ~ 34.61 cycles
	    358,316 per/second average  [traceEvent]                ~ 4,889 cycles
    Finally, doing a test on 4 threads -- HYPERTHREADING
	 34,021,584 per/second average  [IORef incrs]               ~ 39.58 cycles
	    246,782 per/second average  [traceEvent]                ~ 5,457 cycles

  Westmere 3.1 ghz 4core noHT:
    First doing a test on one thread:
	 74,393,471 per/second average  [IORef incrs]               ~ 40.56 cycles
	    205,615 per/second average  [traceEvent]                ~ 14,673 cycles
    Doing a test on 2 threads at once 
	 76,597,784 per/second average  [IORef incrs]               ~ 39.39 cycles
	     34,764 per/second average  [traceEvent]                ~ 86,786 cycles
    Doing a test on all 4 threads at once 
	 71,818,103 per/second average  [IORef incrs]               ~ 39.02 cycles
	     10,360 per/second average  [traceEvent]                ~ 270,485 cycles

-}
