{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns, ScopedTypeVariables, NamedFieldPuns, CPP #-}

-- Various utilities used during testing.

module CommonTesting  where

import Control.Monad
import Control.Concurrent.MVar
import GHC.Conc
import Data.Time.Clock
import Text.Printf
import GHC.IO (unsafePerformIO)
import System.CPUTime
import System.Mem.StableName (makeStableName, hashStableName)
import System.Environment (getEnvironment)
import System.IO        (stdout, stderr, hPutStrLn, hFlush)
import Debug.Trace      (trace)

-- import Test.Framework.TH (defaultMainGenerator)


----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

checkGCStats :: IO ()
checkGCStats = return ()
    -- do b <- getGCStatsEnabled
    --    unless b $ error "Cannot run tests without +RTS -T !!"

dotdot :: Int -> String -> String
dotdot len chars = 
  if length chars > len
  then take len chars ++ "..."
  else chars

printBits :: [Bool] -> IO ()
printBits = print . map pb
 where pb True  = '1' 
       pb False = '0'

forkJoin :: Int -> (Int -> IO b) -> IO [b]
forkJoin numthreads action = 
  do
     answers <- sequence (replicate numthreads newEmptyMVar) -- padding?
     dbgPrint 1 $ printf "Forking %d threads.\n" numthreads
    
     forM_ (zip [0..] answers) $ \ (ix,mv) -> 
 	forkIO (action ix >>= putMVar mv)

     -- Reading answers:
     ls <- mapM readMVar answers
     dbgPrint 1 $ printf "All %d thread(s) completed\n" numthreads
     return ls

-- TODO: Here's an idea.  Describe a structure of forking and joining threads for
-- tests, then we can stress test it by running different interleavings explicitly.
data Forkable a = Fork Int (IO a)
                | Parallel (Forkable a) (Forkable a) -- Parallel composition
                | Sequence (Forkable a) (Forkable a) -- Sequential compositon, with barrier
--                | Barrier Forkable

-- | Grab a GC-invariant stable "address" for any value.
{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)


-- | Measure realtime 
timeit :: IO a -> IO a 
timeit ioact = do 
   start <- getCurrentTime
   res <- ioact
   end   <- getCurrentTime
   putStrLn$ "  Time elapsed: " ++ show (diffUTCTime end start)
   return res


-- | Measure CPU time rather than realtime...
cputime :: IO t -> IO t
cputime a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "SELFTIMED: %0.3f sec\n" (diff :: Double)
    return v


-- | To make sure we get a simple loop...
nTimes :: Int -> IO () -> IO ()
-- nTimes :: Int -> IO a -> IO ()
-- Note: starting out I would get 163Mb allocation for 10M sequential incrs (on unboxed).
-- The problem was that the "Int" result from each incr was being allocated.
-- Weird thing is that inlining nTimes reduces the allocation to 323Mb.
-- But forcing it to take an (IO ()) gets rid of the allocation.
-- Egad, wait, no, I have to NOT inline nTimes to get rid of the allocation!?!?
-- Otherwise I'm still stuck with at least 163Mb of allocation.
-- In fact... the allocation is still there even if we use incrCounter_ !!
-- If we leave nTimes uninlined, we can get down to 3Mb allocation with either incrCounter or incrCounter_.
-------------------------
-- UPDATE:
-- As per http://www.haskell.org/pipermail/glasgow-haskell-users/2011-June/020472.html
--
--  INLINE should not affect recursive functions.  But here it seems to have a
--  deleterious effect!
nTimes 0 !c = return ()
nTimes !n !c = c >> nTimes (n-1) c



----------------------------------------------------------------------------------------------------
-- DEBUGGING
----------------------------------------------------------------------------------------------------

-- | Debugging flag shared by all accelerate-backend-kit modules.
--   This is activated by setting the environment variable DEBUG=1..5
dbg :: Int
dbg = case lookup "DEBUG" unsafeEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         warnUsing (" DEBUG="++s)$
         case reads s of
           ((n,_):_) -> n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

-- | How many elements or iterations should the test use?
numElems :: Int
numElems = case lookup "NUMELEMS" unsafeEnv of 
             Nothing  -> 1000 * 1000  -- A million by default.
             Just str -> warnUsing ("NUMELEMS = "++str) $ 
                         read str

warnUsing :: String -> a -> a
warnUsing str a = trace ("  [Warning]: Using environment variable "++str) a

defaultDbg :: Int
defaultDbg = 0

unsafeEnv :: [(String,String)]
unsafeEnv = unsafePerformIO getEnvironment

-- | Print if the debug level is at or above a threshold.
dbgPrint :: Int -> String -> IO ()
dbgPrint lvl str = if dbg < lvl then return () else do
    hPutStrLn stderr str
    hFlush stderr

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i == end  = return ()
	   | otherwise = do fn i; loop (i+1)

