{-# LANGUAGE BangPatterns, CPP #-}

-- module Data.Concurrent.LinkedQueue 
module Main

  where

import Control.Monad
import Data.CAS
import Data.IORef
import System.Mem.StableName

-- Considering using the Queue class definition:
-- import Data.MQueue.Class

-- | A straightforward implementation of classic Michael & Scott Queues.
-- 
-- Pseudocode for this algorithm can be found here:
--   http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html


data LinkedQueue a = LQ 
    { head :: IORef (Pair a)
    , tail :: IORef (Pair a)
    }

data Pair a = Null | Cons a (IORef (Pair a))

{-# NOINLINE nullConst #-}
nullConst = Null

push :: LinkedQueue a -> a  -> IO ()
push (LQ headPtr tailPtr) val = do
   r <- newIORef nullConst  
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
     Null -> error "Invariants broken."
     Cons tailCar tailCdr -> do       
	tailCdr' <- readIORef tailCdr
	-- The algorithm rereads tailPtr here to make sure it is still good.
#if 1
	tail' <- readIORef tailPtr
        b <- ptrEq tail tail'
        if (not b) then loop newp 
         else case tailCdr' of 
#else
	case tailCdr' of 
#endif
        -- We skip that simply because comparing pointers would require StableNames.
	 Null -> error "Invariants broken 2."
	 Cons x next -> do
	   next' <- readIORef next
	   case next' of 
	     Null -> do (b,newtail) <- casIORef tailCdr next' next'
-- TODO: an alternative here is rather than reading "tailPtr" again at the top of the loop
-- we could use the "newtail" value to chase the chain one at a time.
-- The question is... if someone beats us here how likely is it that two or more will have beaten us?
			if b then return tail
                             else loop newp
	     Cons _ _ -> do 
		-- We try to bump the tail in this case, but if we don't someone else will.
		casIORef tailCdr tailCdr' next'
		return tail


tryPop ::  LinkedQueue a -> IO (Maybe a)
tryPop = undefined

newLinkedQueue :: IO (LinkedQueue a)
newLinkedQueue = do 
  r <- newIORef Null
  let newp = Cons (error "LinkedQueue: Used uninitialized value.") r
  hd <- newIORef newp
  tl <- newIORef newp
  return (LQ hd tl)

--------------------------------------------------------------------------------

{-# INLINE ptrEq #-}
ptrEq :: a -> a -> IO Bool
ptrEq a b = do 
  s1 <- makeStableName a
  s2 <- makeStableName b
  return (s1 == s2)


--------------------------------------------------------------------------------
-- Scrap:

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
     forM_ [1..1000] $ \i -> 
       push q i
     sum <- newIORef 0
     forM_ [1..1000] $ \i -> do
       let loop = do
	    x <- tryPop q 
            case x of 
	      Nothing -> loop
	      Just x  -> return x
       x <- loop
       modifyIORef sum (+x)
     readIORef sum


main = testCAS
