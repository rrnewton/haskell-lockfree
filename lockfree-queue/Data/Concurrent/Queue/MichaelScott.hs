{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples, ScopedTypeVariables #-}
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

import Data.IORef (readIORef, newIORef)
import System.IO (stderr)
import Data.ByteString.Char8 (hPutStrLn, pack)

-- import GHC.Types (Word(W#))
import GHC.Prim (sameMutVar#)
import GHC.IORef(IORef(IORef))
import GHC.STRef(STRef(STRef))

import qualified Data.Concurrent.Deque.Class as C
import Data.Atomics (readForCAS, casIORef, Ticket, peekTicket)

-- Considering using the Queue class definition:
-- import Data.MQueue.Class

data LinkedQueue a = LQ 
    { head :: {-# UNPACK #-} !(IORef (Pair a))
    , tail :: {-# UNPACK #-} !(IORef (Pair a))
    }

data Pair a = Null | Cons a {-# UNPACK #-}!(IORef (Pair a))

{-# INLINE pairEq #-}
-- | This only checks that the node type is the same and in the case of a Cons Pair
-- checks that the underlying MutVar#s are pointer-equal. This suffices to check
-- equality since each IORef is never used in multiple Pair values.
pairEq :: Pair a -> Pair a -> Bool
pairEq Null       Null        = True
pairEq (Cons _ (IORef (STRef mv1)))
       (Cons _ (IORef (STRef mv2))) = sameMutVar# mv1 mv2
pairEq _          _           = False

-- | Push a new element onto the queue.  Because the queue can grow,
--   this always succeeds.
pushL :: forall a . LinkedQueue a -> a  -> IO ()
pushL q@(LQ headPtr tailPtr) val = do
   r <- newIORef Null
   let newp = Cons val r   -- Create the new cell that stores val.
       -- Enqueue loop: repeatedly read the tail pointer and attempt to extend the last pair.
       loop :: IO ()
       loop = do 
        tailTicket <- readForCAS tailPtr -- [Re]read the tailptr from the queue structure.
        case peekTicket tailTicket of
          -- The head and tail pointers should never themselves be NULL:
          Null -> error "push: LinkedQueue invariants broken.  Internal error."
          Cons _ nextPtr -> do
             nextTicket <- readForCAS nextPtr

     -- The algorithm can reread tailPtr here to make sure it is still good:
     -- [UPDATE: This is actually a necessary part of the algorithm's "hand-over-hand"
     --  locking, NOT an optimization.]
#ifdef RECHECK_ASSUMPTIONS
      -- There's a possibility for an infinite loop here with StableName based ptrEq.
      -- (And at one point I observed such an infinite loop.)
      -- But with one based on reallyUnsafePtrEquality# we should be ok.
             (tailTicket', tail') <- readForCAS tailPtr   -- ANDREAS: used atomicModifyIORef here
             if not (pairEq tail tail') then loop
              else case next of 
#else
             case peekTicket nextTicket of 
#endif
               -- Here tail points (or pointed!) to the last node.  Try to link our new node.
               Null -> do (b,newtick) <- casIORef nextPtr nextTicket newp
                          case b of 
                            True -> do 
                              --------------------Exit Loop------------------
                              -- After the loop, enqueue is done.  Try to swing the tail.
                              -- If we fail, that is ok.  Whoever came in after us deserves it.
                              _ <- casIORef tailPtr tailTicket newp
                              return ()
                              -----------------------------------------------
                            False -> loop 
               nxt@(Cons _ _) -> do 
                  -- Someone has beat us by extending the tail.  Here we
                  -- might have to do some community service by updating the tail ptr.
                  _ <- casIORef tailPtr tailTicket nxt
                  loop 

   loop -- Start the loop.

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
tryPopR :: forall a . LinkedQueue a -> IO (Maybe a)
-- FIXME -- this version
-- TODO -- add some kind of backoff.  This should probably at least
-- yield after a certain number of failures.
tryPopR q@(LQ headPtr tailPtr) = loop 0
 where
  loop :: Int -> IO (Maybe a)
#ifdef DEBUG
   --  loop 10 = do hPutStrLn stderr (pack "tryPopR: tried ~10 times!!");  loop 11 -- This one happens a lot on -N32
  loop 25   = do hPutStrLn stderr (pack "tryPopR: tried ~25 times!!");   loop 26
  loop 50   = do hPutStrLn stderr (pack "tryPopR: tried ~50 times!!");   loop 51
  loop 100  = do hPutStrLn stderr (pack "tryPopR: tried ~100 times!!");  loop 101
  loop 1000 = do hPutStrLn stderr (pack "tryPopR: tried ~1000 times!!"); loop 1001
#endif
  loop !tries = do 
    headTicket <- readForCAS headPtr
    tailTicket <- readForCAS tailPtr
    case peekTicket headTicket of 
      Null -> error "tryPopR: LinkedQueue invariants broken.  Internal error."
      head@(Cons _ next) -> do
        nextTicket' <- readForCAS next
#ifdef RECHECK_ASSUMPTIONS
        -- As with push, double-check our information is up-to-date. (head,tail,next consistent)
        head' <- readIORef headPtr -- ANDREAS: used atomicModifyIORef headPtr (\x -> (x,x))
        if not (pairEq head head') then loop (tries+1) else do 
#else
        let head' = head
        do 
#endif                 
	  -- Is queue empty or tail falling behind?:
          if pairEq head (peekTicket tailTicket) then do 
          -- if ptrEq head tail then do 
	    case peekTicket nextTicket' of -- Is queue empty?
              Null -> return Nothing -- Queue is empty, couldn't dequeue
	      next'@(Cons _ _) -> do
  	        -- Tail is falling behind.  Try to advance it:
	        casIORef tailPtr tailTicket next'
		loop (tries+1)
           
	   else do -- head /= tail
	      -- No need to deal with Tail.  Read value before CAS.
	      -- Otherwise, another dequeue might free the next node
	      case peekTicket nextTicket' of 
	        Null -> error "tryPop: Internal error.  Next should not be null if head/=tail."
--	        Null -> loop (tries+1)
		next'@(Cons value _) -> do 
                  -- Try to swing Head to the next node
		  (b,_) <- casIORef headPtr headTicket next'
                  case b of
                    -- [2013.04.24] Looking at the STG, I can't see a way to get rid of the allocation on this Just:
                    True  -> return (Just value) -- Dequeue done; exit loop.
                    False -> loop (tries+1) -- ANDREAS: observed this loop being taken >1M times
          
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
  leftThreadSafe _  = True
  rightThreadSafe _ = True

--------------------------------------------------------------------------------
