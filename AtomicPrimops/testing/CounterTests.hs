
-- | Test the counter implementation alternatives.

module CounterTests where

import Test.Framework.Providers.HUnit (testCase)
import Text.Printf
import System.CPUTime  
import Data.IORef  

import Data.Atomics
import qualified Data.Atomics.Counter.Reference as C1 
import qualified Data.Atomics.Counter.IORef     as C2
import qualified Data.Atomics.Counter.Foreign   as C3

import CommonTesting (numElems)

--------------------------------------------------------------------------------

-- | To make sure we get a simple loop...
nTimes :: Int -> IO a -> IO ()
nTimes 0 c = return ()
nTimes n c = c >> nTimes (n-1) c

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "SELFTIMED: %0.3f sec\n" (diff :: Double)
    return v

normal tries = do
  r <- newIORef True
  nTimes tries $ do
    x <- readIORef r
    writeIORef r $! not x
    
casBased tries = do    
  r <- newIORef True
  nTimes tries $ do
    t <- readForCAS r
    casIORef r t (not $ peekTicket t)
    return ()
  readIORef r

normalIncr tries = do
  r <- newIORef 0
  nTimes tries $ do
    x <- readIORef r
    writeIORef r $! 1 + x
  readIORef r

casBasedIncr tries = do    
  r <- newIORef 0
  nTimes tries $ do
    t <- readForCAS r
    casIORef r t $! (1 + peekTicket t)
    return ()
  readIORef r

incrloop1 tries = do
  r <- C1.newCounter 0
  nTimes tries $ C1.incrCounter 1 r

incrloop2 tries = do
  r <- C2.newCounter 0
  nTimes tries $ C2.incrCounter 1 r

incrloop3 tries = do
  r <- C3.newCounter 0
  nTimes tries $ C3.incrCounter 1 r
  
--------------------------------------------------------------------------------

-- default_tries = 10 *1000*1000
default_tries = numElems

counterTests = 
 [
   ----------------------------------------
   testCase "RAW_single_thread_repeat_flip" $ do 
     putStrLn "Timing readIORef/writeIORef on one thread"
     time (normal default_tries)   
 , testCase "CAS_single_thread_repeat_flip" $ do 
     putStrLn "Timing CAS boolean flips on one thread without retries"
     fin <- time (casBased default_tries)
     putStrLn$"Final value: "++show fin
   ----------------------------------------
 , testCase "RAW_single_thread_repeat_incr" $ do 
     putStrLn "Timing readIORef/writeIORef on one thread"
     fin <- time (normalIncr default_tries)
     putStrLn$"Final value: "++show fin      
 , testCase "CAS_single_thread_repeat_incr" $ do 
     putStrLn "Timing CAS increments on one thread without retries"
     fin <- time (casBasedIncr default_tries)
     putStrLn$"Final value: "++show fin 
   ----------------------------------------
 , testCase "CounterReference_single_thread_repeat_incr" $ time (incrloop1 default_tries)
 , testCase "CounterIORef_single_thread_repeat_incr"     $ time (incrloop2 default_tries)   
 , testCase "CounterForeign_single_thread_repeat_incr"   $ time (incrloop3 default_tries)   
   
 ]

