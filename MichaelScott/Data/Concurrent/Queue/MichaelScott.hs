{-# LANGUAGE BangPatterns, CPP  #-}
-- TypeFamilies, FlexibleInstances

-- | Michael and Scott lock-free, single-ended queues.
-- 
-- This is a straightforward implementation of classic Michael & Scott Queues.
-- Pseudocode for this algorithm can be found here:
-- 
--   <http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html>

module Data.Concurrent.Queue.MichaelScott
 (
   -- The convention here is to directly provide the concrete
   -- operations as well as providing the class instances.
   LinkedQueue(), newQ, nullQ, pushL, tryPopR, 
 )
  where

import Control.Monad
import Data.IORef
import System.Mem.StableName
import Text.Printf
import GHC.IO (unsafePerformIO)
import GHC.Conc
import Control.Concurrent.MVar

import qualified Data.Concurrent.Deque.Class as C

import Data.CAS (casIORef, ptrEq)
-- import Data.CAS.Internal.Fake (casIORef, ptrEq)
-- #warning "Using fake CAS"
-- import Data.CAS.Internal.Native (casIORef, ptrEq)
-- #warning "Using NATIVE CAS"


-- Considering using the Queue class definition:
-- import Data.MQueue.Class28

data LinkedQueue a = LQ 
    { head :: IORef (Pair a)
    , tail :: IORef (Pair a)
    }

data Pair a = Null | Cons a (IORef (Pair a))


-- | Push a new element onto the queue.  Because the queue can grow,
--   this always succeeds.
pushL :: LinkedQueue a -> a  -> IO ()
pushL (LQ headPtr tailPtr) val = do
   r <- newIORef Null
   let newp = Cons val r   -- Create the new cell that stores val.
   tail <- loop newp
   -- After the loop, enqueue is done.  Try to swing the tail.
   -- If we fail, that is ok.  Whoever came in after us deserves it.
   casIORef tailPtr tail newp
   return ()
 where 
  loop newp = do 
   tail <- readIORef tailPtr -- Reread the tailptr from the queue structure.
   case tail of 
     Null -> error "push: LinkedQueue invariants broken.  Internal error."
     Cons _ next -> do
	next' <- readIORef next
	-- The algorithm rereads tailPtr here to make sure it is still good.
#if 1
 -- There's a possibility for an infinite loop here with StableName based ptrEq.
 -- (And at one point I observed such an infinite loop.)
 -- But with one based on reallyUnsafePtrEquality# we should be ok.
	tail' <- readIORef tailPtr
        if not (ptrEq tail tail') then loop newp 
         else case next' of 
#else
	case next' of 
#endif
          -- We skip that simply because comparing pointers would require StableNames.
          Null -> do (b,newtail) <- casIORef next next' newp
		     if b then return tail
                          else loop newp
          Cons _ _ -> do 
	     -- We try to bump the tail in this case, but if we don't someone else will.
	     casIORef next next' newp
	     return tail


-- | Attempt to pop an element from the queue if one is available.
--   tryPop will always return promptly, but will return 'Nothing' if
--   the queue is empty.
tryPopR ::  LinkedQueue a -> IO (Maybe a)
tryPopR (LQ headPtr tailPtr) = loop
 where 
  loop = do 
    head <- readIORef headPtr
    tail <- readIORef tailPtr
    case head of 
      Null -> error "tryPopR: LinkedQueue invariants broken.  Internal error."
      Cons _ next -> do
        next' <- readIORef next
        -- As with push, double-check our information is up-to-date. (head,tail,next consistent)
        head' <- readIORef headPtr
        if not (ptrEq head head') then loop else do 
	  -- Is queue empty or tail falling behind?:
          if ptrEq head tail then do 
	    case next' of -- Is queue empty?
              Null -> return Nothing -- Queue is empty, couldn't dequeue
	      Cons _ _ -> do
  	        -- Tail is falling behind.  Try to advance it:
	        casIORef tailPtr tail next'
		loop 
           
	   else do -- head /= tail
	      -- No need to deal with Tail.  Read value before CAS.
	      -- Otherwise, another dequeue might free the next node
	      case next' of 
--	        Null -> error "tryPop: Internal error.  Next should not be null if head/=tail."
	        Null -> loop 
		Cons value _ -> do 
                  -- Try to swing Head to the next node
		  (b,_) <- casIORef headPtr head next'
		  if b then return (Just value) -- Dequeue done; exit loop.
		       else loop   
          
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
    return (ptrEq head tail)



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
--   Notes
--------------------------------------------------------------------------------
{- 
[2011.10.29] {Debugging}

Currently segfaulting.  GDB says:

  #0  0x00000001000492e8 in base_GHCziReal_zdfIntegralIntzuzdctoInteger_info ()
  #1  0x0000000000000000 in ?? ()

Valgrind on the other hand says: 

    ==16469== Invalid read of size 8
    ==16469==    at 0x52E349: base_GHCziSTRef_readSTRef1_info (in /nfs/nfs3/home/rrnewton/working_copies/haskell-lockfree/test.exe)
    ==16469==  Address 0x0 is not stack'd, malloc'd or (recently) free'd
    ==16469== 
    ==16469== 
    ==16469== Process terminating with default action of signal 11 (SIGSEGV)
    ==16469==  Access not within mapped region at address 0x0
    ==16469==    at 0x52E349: base_GHCziSTRef_readSTRef1_info (in /nfs/nfs3/home/rrnewton/working_copies/haskell-lockfree/test.exe)
    ==16469==  If you believe this happened as a result of a stack
    ==16469==  overflow in your program's main thread (unlikely but
    ==16469==  possible), you can try to increase the size of the
    ==16469==  main thread stack using the --main-stacksize= flag.
    ==16469==  The main thread stack size used in this run was 10485760.
    ==16469== 

Looks like a null pointer dereference.
In GHCI sometimes it segfaults and sometimes I get this error:

    Done filling queue with elements.  Now popping...
    <interactive>: internal error: ARR_WORDS object entered!
	(GHC version 7.2.1 for x86_64_unknown_linux)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

 -}
