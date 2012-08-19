{-# LANGUAGE BangPatterns, CPP #-}
-- TypeFamilies, FlexibleInstances

-- | Michael and Scott lock-free, single-ended queues.
-- 
-- This is a straightforward implementation of classic Michael & Scott Queues.
-- Pseudocode for this algorithm can be found here:
-- 
--   <http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html>

-- Uncomment this if desired.  Needs more testing:
-- #define RECHECK_ASSUMPTIONS

module Data.Concurrent.Queue.MichaelScott
 (
   -- The convention here is to directly provide the concrete
   -- operations as well as providing the typeclass instances.
   LinkedQueue(), newQ, nullQ, pushL, tryPopR, 
 )
  where

import Data.IORef (readIORef, IORef, newIORef)
import System.IO (stderr)
import Data.ByteString.Char8 (hPutStrLn, pack)

import qualified Data.Concurrent.Deque.Class as C
-- NOTE: you can switch which CAS implementation is used here:
--------------------------------------------------------------
import Data.CAS (casIORef, ptrEq)
-- import Data.CAS.Internal.Fake (casIORef, ptrEq)
-- #warning "Using fake CAS"
-- import Data.CAS.Internal.Native (casIORef, ptrEq)
-- #warning "Using NATIVE CAS"
--------------------------------------------------------------

-- Considering using the Queue class definition:
-- import Data.MQueue.Class

data LinkedQueue a = LQ 
    { head :: IORef (Pair a)
    , tail :: IORef (Pair a)
    }

data Pair a = Null | Cons a (IORef (Pair a))

-- Only checks that the node type is the same and in the case of a Cons Pair checks that
-- the IORefs are pointer-equal. This suffices to check equality since IORefs are never used in different 
-- Pair values.
pairEq :: Pair a -> Pair a -> Bool
pairEq Null       Null        = True
pairEq (Cons _ r) (Cons _ r') = ptrEq r r'
pairEq _          _           = False

-- | Push a new element onto the queue.  Because the queue can grow,
--   this always succeeds.
pushL :: LinkedQueue a -> a  -> IO ()
pushL q@(LQ headPtr tailPtr) val = do
   r <- newIORef Null
   let newp = Cons val r   -- Create the new cell that stores val.
   tail <- loop newp
   -- After the loop, enqueue is done.  Try to swing the tail.
   -- If we fail, that is ok.  Whoever came in after us deserves it.
   casIORef tailPtr tail newp
   return ()
 where 
  loop newp = do 
   tail <- readIORef tailPtr -- [Re]read the tailptr from the queue structure.
   case tail of
     -- The head and tail pointers should never themselves be NULL:
     Null -> error "push: LinkedQueue invariants broken.  Internal error."
     Cons _ nextPtr -> do
	next <- readIORef nextPtr

-- Optimization: The algorithm can reread tailPtr here to make sure it is still good:
#ifdef RECHECK_ASSUMPTIONS
 -- There's a possibility for an infinite loop here with StableName based ptrEq.
 -- (And at one point I observed such an infinite loop.)
 -- But with one based on reallyUnsafePtrEquality# we should be ok.
	tail' <- readIORef tailPtr   -- ANDREAS: used atomicModifyIORef here
        if not (pairEq tail tail') then loop newp 
         else case next of 
#else
	case next of 
#endif
          -- Here tail points (or pointed!) to the last node.  Try to link our new node.
          Null -> do (b,newtail) <- casIORef nextPtr next newp
		     if b then return tail
                          else loop newp
          Cons _ _ -> do 
             -- Someone has beat us by extending the tail.  Here we
             -- might have to do some community service by updating the tail ptr.
             casIORef tailPtr tail next
             loop newp


-- Andreas's checked this invariant in several places
-- Check for: head /= tail, and head->next == NULL
checkInvariant :: String -> LinkedQueue a -> IO ()
checkInvariant s (LQ headPtr tailPtr) = 
  do head <- readIORef headPtr
     tail <- readIORef tailPtr
     if (not (pairEq head tail))
       then case head of 
              Null -> error (s ++ " checkInvariant: LinkedQueue invariants broken.  Internal error.")
              Cons _ next -> do
                next' <- readIORef next
                case next' of 
                  Null -> error (s ++ " checkInvariant: next' should not be null")
                  _ -> return ()
       else return ()
            
-- | Attempt to pop an element from the queue if one is available.
--   tryPop will return semi-promptly (depending on contention), but
--   will return 'Nothing' if the queue is empty.
tryPopR ::  LinkedQueue a -> IO (Maybe a)
-- FIXME -- this version
-- TODO -- add some kind of backoff.  This should probably at least
-- yield after a certain number of failures.
tryPopR q@(LQ headPtr tailPtr) = loop (0::Int) 
 where 
#ifdef DEBUG
   --  loop 10 = do hPutStrLn stderr (pack "tryPopR: tried ~10 times!!");  loop 11 -- This one happens a lot on -N32
  loop 25   = do hPutStrLn stderr (pack "tryPopR: tried ~25 times!!");   loop 26
  loop 50   = do hPutStrLn stderr (pack "tryPopR: tried ~50 times!!");   loop 51
  loop 100  = do hPutStrLn stderr (pack "tryPopR: tried ~100 times!!");  loop 101
  loop 1000 = do hPutStrLn stderr (pack "tryPopR: tried ~1000 times!!"); loop 1001
#endif
  loop !tries = do 
    head <- readIORef headPtr
    tail <- readIORef tailPtr
    case head of 
      Null -> error "tryPopR: LinkedQueue invariants broken.  Internal error."
      Cons _ next -> do
        next' <- readIORef next
#ifdef RECHECK_ASSUMPTIONS
        -- As with push, double-check our information is up-to-date. (head,tail,next consistent)
        head' <- readIORef headPtr -- ANDREAS: used atomicModifyIORef headPtr (\x -> (x,x))
        if not (pairEq head head') then loop (tries+1) else do 
#else
        let head' = head
        do 
#endif                 
	  -- Is queue empty or tail falling behind?:
          if pairEq head tail then do 
          -- if ptrEq head tail then do 
	    case next' of -- Is queue empty?
              Null -> return Nothing -- Queue is empty, couldn't dequeue
	      Cons _ _ -> do
  	        -- Tail is falling behind.  Try to advance it:
	        casIORef tailPtr tail next'
		loop (tries+1)
           
	   else do -- head /= tail
	      -- No need to deal with Tail.  Read value before CAS.
	      -- Otherwise, another dequeue might free the next node
	      case next' of 
	        Null -> error "tryPop: Internal error.  Next should not be null if head/=tail."
--	        Null -> loop (tries+1)
		Cons value _ -> do 
                  -- Try to swing Head to the next node
		  (b,_) <- casIORef headPtr head next' -- ANDREAS: FOUND CONDITION VIOLATED AFTER HERE
		  if b then return (Just value) -- Dequeue done; exit loop.
		       else loop (tries+1) -- ANDREAS: observed this loop being taken >1M times
          
-- | Create a new queue.
newQ :: IO (LinkedQueue a)
newQ = do 
  r <- newIORef Null
  let newp = Cons (error "LinkedQueue: Used uninitialized magic value.") r
  hd <- newIORef newp
  tl <- newIORef newp
  return (LQ hd tl)

-- | Is the queue currently empty?  Beware that this can be a highly transient state.
nullQ :: LinkedQueue a -> IO Bool
nullQ (LQ headPtr tailPtr) = do 
    head <- readIORef headPtr
    tail <- readIORef tailPtr
    return (pairEq head tail)



--------------------------------------------------------------------------------
--   Instance(s) of abstract deque interface
--------------------------------------------------------------------------------

-- instance DequeClass (Deque T T S S Grow Safe) where 
instance C.DequeClass LinkedQueue where 
  newQ    = newQ
  nullQ   = nullQ
  pushL   = pushL
  tryPopR = tryPopR

--------------------------------------------------------------------------------
