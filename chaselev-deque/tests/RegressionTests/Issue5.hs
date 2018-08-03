module RegressionTests.Issue5 (standalone_pushPop) where

import Control.Concurrent
import Data.IORef
import Data.Concurrent.Deque.Class 
import qualified Data.Concurrent.Deque.ChaseLev as CL

--------------------------------------------------------------------------------

standalone_pushPop :: IO ()
standalone_pushPop =
  triv =<< (newQ :: IO (DebugDeque CL.ChaseLevDeque a))           
 where   
   -- This is what's failing with the debug wrapper, WHY?
   triv :: PopL d => d [Char] -> IO ()
   triv q = do
     pushL q "hi" 
     x <- tryPopL q
     case x of
       Just "hi" -> putStrLn "Got expected value.  Test passed.\n"
       Just x'   -> error$ "A single push/pop got the WRONG value back: "++show x'
       Nothing   -> error "Even a single push/pop in isolation did not work!"


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
    pushL q elt

  tryPopR (DebugDeque (_,ref) q) = do
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
    tryPopL q 

