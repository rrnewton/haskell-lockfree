
-- This test was formulated to exercise a bug that Andreas found.

-- TODO: Move this to the normal test suite.

import Control.Concurrent
import Data.Concurrent.Queue.MichaelScott
import Data.Array.IArray
import System.Random.MWC hiding (Seed)
import qualified Data.Vector as V

-- import GHC.IO.Handle (hFlush, stdout)
import System.IO (stdout, hFlush)
import Debug.Trace (trace)

main = do 
  qarray <- fmap (listArray (0, 99)) (sequence (replicate 100 newQ))
  -- Fork 100 threads:
  vars <- mapM (\i -> newEmptyMVar >>= \var -> forkIO (prog i qarray var) >> return var) [0..99]
  mapM_ takeMVar vars


numIters :: Int
-- numIters = 100000
numIters = 10000
-- numIters = 30000

prog :: Int -> Array Int (LinkedQueue Int) -> MVar () -> IO ()
prog i qarray done = go numIters
  where
    go j | j > 0 && j `mod` 1000 == 0 =
      do putStr "."
         hFlush stdout
         go (j-1)
    go j = if j==0
           then putMVar done ()
           else do gen <- initialize (V.singleton (fromIntegral i))
                   ix <- uniformR (0,99) gen
                   pushL (qarray ! ix) i
                   popUntilNothing (qarray ! i)
                   go (j-1)
    popUntilNothing myq 
      = do res <- tryPopR myq
           case res of 
             Nothing -> return ()
             Just _  -> popUntilNothing myq
