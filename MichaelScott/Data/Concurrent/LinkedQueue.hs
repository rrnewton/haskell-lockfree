{-# LANGUAGE BangPatterns, CPP  #-}
-- TypeFamilies, FlexibleInstances

-- | Michael and Scott lock-free, wait-free, single-ended queues.
-- module Main
module Data.Concurrent.LinkedQueue 
 (
   newLinkedQueue
 , push 
 , tryPop
 , LinkedQueue()
 )
  where

import Control.Monad
import Data.IORef
import System.Mem.StableName
import Text.Printf
import GHC.IO (unsafePerformIO)
import GHC.Conc
import Control.Concurrent.MVar

import Data.Concurrent.Deque.Class

import Data.CAS.Fake (ptrEq)
#if 0
-- Segfaulting currently:
import Data.CAS (casIORef)
#else
import Data.CAS.Fake (casIORef)
#endif

-- Considering using the Queue class definition:
-- import Data.MQueue.Class28


-- | A straightforward implementation of classic Michael & Scott Queues.
-- 
-- Pseudocode for this algorithm can be found here:
--   http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html

data LinkedQueue a = LQ 
    { head :: IORef (Pair a)
    , tail :: IORef (Pair a)
    }

data Pair a = Null | Cons a (IORef (Pair a))

-- | Push a new element onto the queue.  Because the queue can grow,
--   this alway succeeds.
push :: LinkedQueue a -> a  -> IO ()
push (LQ headPtr tailPtr) val = do
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
   case tail of -- Next let's examine that tail ptr and see if it's really the end.
     -- We skip that simply because comparing pointers would require StableNames.
     Null -> error "push: LinkedQueue invariants broken.  Internal error."
     Cons _ next -> do
	next' <- readIORef next
	-- The algorithm rereads tailPtr here to make sure it is still good.
#if 1
 -- Ack, actually the following will result in an infinite loop!
 -- StableName's don't GUARANTEE that equal pointers return TRUE
-- [2011.10.29] Umm... the infinite loop went away... No idea.
	tail' <- readIORef tailPtr
        b <- ptrEq tail tail'
        if (not b) then loop newp 
         else case next' of 
#else
	case next' of 
#endif
          -- We skip that simply because comparing pointers would require StableNames.
          Null -> do (b,newtail) <- casIORef next next' newp
-- TODO: an alternative here is rather than reading "tailPtr" again at the top of the loop
-- we could use the "newtail" value to chase the chain one at a time.
-- The question is... if someone beats us here how likely is it that two or more will have beaten us?
		     if b then return tail
                          else loop newp
          Cons _ _ -> do 
	     -- We try to bump the tail in this case, but if we don't someone else will.
	     casIORef next next' newp
	     return tail


-- | Attempt to pop an element from the queue if one is available.
--   tryPop will always return promptly, but will return 'Nothing' if
--   the queue is empty.
tryPop ::  LinkedQueue a -> IO (Maybe a)
tryPop (LQ headPtr tailPtr) = loop
 where 
  loop = do 
    head <- readIORef headPtr
    tail <- readIORef tailPtr
    case head of 
      Null -> error "tryPop: LinkedQueue invariants broken.  Internal error."
      Cons _ next -> do
        next' <- readIORef next
        -- As with push, double-check our information is up-to-date. (head,tail,next consistent)
        head' <- readIORef headPtr
        b <- ptrEq head head' 
        if (not b) then loop else do 
	  b <- ptrEq head tail -- Is queue empty or tail falling behind?
          if b then do 
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
          

newLinkedQueue :: IO (LinkedQueue a)
newLinkedQueue = do 
  r <- newIORef Null
  let newp = Cons (error "LinkedQueue: Used uninitialized magic value.") r
  hd <- newIORef newp
  tl <- newIORef newp
  return (LQ hd tl)


--------------------------------------------------------------------------------
--   Instances of abstract deque interface
--------------------------------------------------------------------------------

-- instance DequeClass (Deque T T S S Grow Safe) where 
instance DequeClass LinkedQueue where 
  newQ    = newLinkedQueue
  pushL   = push
  tryPopR = tryPop

--------------------------------------------------------------------------------
--   Testing
--------------------------------------------------------------------------------

spinPop q = do
  x <- tryPop q 
  case x of 
    Nothing -> spinPop q
    Just x  -> return x

casStrict r !o !n = casIORef r o n

testCAS = 
  do let zer = (0::Int)
     r <- newIORef zer
     let loop 0 = return ()
	 loop n = do
          (b,v) <- casIORef r zer 100  -- Must use "zer" here.
--          (b,v) <- casStrict r 0 100  -- Otherwise this is nondeterministic based on compiler opts.
		   -- Sometimes the latter version works on the SECOND evaluation of testCAS.  Interesting.
          putStrLn$ "After CAS " ++ show (b,v)
          loop (n-1)
     loop 10 
     return ()

testQ1 = 
  do q <- newLinkedQueue 
     let n = 1000
     putStrLn$ "Done creating queue.  Pushing elements:"
     forM_ [1..n] $ \i -> do 
       push q i
       printf " %d" i
     putStrLn "\nDone filling queue with elements.  Now popping..."
     sumR <- newIORef 0
     forM_ [1..n] $ \i -> do
       x <- spinPop q 
       printf " %d" x
       modifyIORef sumR (+x)
     s <- readIORef sumR
     let expected = sum [1..n] :: Int
     printf "\nSum of popped vals: %d should be %d\n" s expected
     when (s /= expected) (error "Incorrect sum!")
     return s

-- This one splits the numCapabilities threads into producers and consumers
testQ2 :: Int -> IO ()
testQ2 total = 
  do q <- newLinkedQueue
     mv <- newEmptyMVar     
     let producers = max 1 (numCapabilities `quot` 2)
	 consumers = producers
	 perthread = total `quot` producers

     printf "Forking %d producer threads.\n" producers 
    
     forM_ [0..producers-1] $ \ id -> 
 	forkIO $ 
          forM_ (take perthread [id * producers .. ]) $ \ i -> do 
	     push q i
             printf " [%d] pushed %d \n" id i

     printf "Forking %d consumer threads.\n" consumers

     forM_ [0..consumers-1] $ \ id -> 
 	forkIO $ do 
          sum <- newIORef 0
          forM_ (take perthread [id * producers .. ]) $ \ i -> do
	     x <- spinPop q 
             printf " [%d] popped %d \n" id i
	     modifyIORef sum (+x)
	  s <- readIORef sum
	  putMVar mv s

     printf "Reading sums from MVar...\n" 
     ls <- mapM (\_ -> takeMVar mv) [1..consumers]
     let finalSum = Prelude.sum ls
     putStrLn$ "Final sum: "++ show finalSum
     return ()

-- main = testCAS
-- main = testQ2 (1000 * 1000)
main = testQ2 (10)


--------------------------------------------------------------------------------
-- TODO: Instances.  Implement the abstract-deque interface.


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
