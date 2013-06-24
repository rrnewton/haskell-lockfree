module RegressionTests.Issue5 (standalone_single_CAS, main) where

import Control.Concurrent
import Data.IORef
import Data.Concurrent.Deque.Class 
import qualified Data.Concurrent.Deque.ChaseLev as CL

main :: IO ()
main = standalone_single_CAS

--------------------------------------------------------------------------------

standalone_single_CAS :: IO ()
standalone_single_CAS =
  triv =<< (newQ :: IO (DebugDeque CL.ChaseLevDeque a))           
 where   
   -- This is what's failing with the debug wrapper, WHY?
   triv :: PopL d => d [Char] -> IO ()
   triv q = do
     pushL q "hi" 
     x <- tryPopL q
     case x of
       Just "hi" -> return ()
       Just x'   -> error$ "A single CAS got the WRONG value back: "++show x'
       Nothing   -> error "Even a single CAS in isolation did not work!"


-- | Warning, this enforces the excessively STRONG invariant that if any end of the
-- deque is non-threadsafe then it may ever only be touched by one thread during its
-- entire lifetime.
--
-- This extreme form of monagamy is easier to verify, because we don't have enough
-- information to know if two operations on different threads are racing with one
-- another or are properly synchronized.
--
-- The wrapper data structure has two IORefs to track the last thread that touched
-- the left and right end of the deque, respectively.
data DebugDeque d elt = DebugDeque (IORef (Maybe ThreadId), IORef (Maybe ThreadId)) (d elt) 


instance DequeClass d => DequeClass (DebugDeque d) where 
  pushL (DebugDeque (ref,_) q) elt = do
    markThread (leftThreadSafe q) ref
    pushL q elt

  tryPopR (DebugDeque (_,ref) q) = do
    markThread (rightThreadSafe q) ref
    tryPopR q 

  newQ = do l <- newIORef Nothing
            r <- newIORef Nothing
            fmap (DebugDeque (l,r)) newQ

  -- FIXME: What are the threadsafe rules for nullQ?
  nullQ (DebugDeque _ q) = nullQ q
      
  leftThreadSafe  (DebugDeque _ q) = leftThreadSafe q
  rightThreadSafe (DebugDeque _ q) = rightThreadSafe q


instance PopL d => PopL (DebugDeque d) where 
  tryPopL (DebugDeque (ref,_) q) = do
    markThread (leftThreadSafe q) ref
    tryPopL q 

-- | Mark the last thread to use this endpoint.
markThread True _ = return () -- Don't bother tracking.
markThread False ref = do
  last <- readIORef ref
  tid  <- myThreadId
--  putStrLn$"Marking! "++show tid
  atomicModifyIORef ref $ \ x ->
    case x of
      Nothing -> (Just tid, ())
      Just tid2
        | tid == tid2 -> (Just tid,())
        | otherwise   -> error$ "DebugDeque: invariant violated, thread safety not allowed but accessed by: "++show (tid,tid2)

