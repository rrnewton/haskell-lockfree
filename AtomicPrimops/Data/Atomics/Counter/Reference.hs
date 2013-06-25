{-# LANGUAGE BangPatterns #-}

-- | This reference version is implemented with atomicModifyIORef and can be a useful
-- fallback if one of the other implementations needs to be debugged.
module Data.Atomics.Counter.Reference
       where

import Data.IORef
-- import Data.Atomics
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- type AtomicCounter = IORef Int
newtype AtomicCounter = AtomicCounter (IORef Int)

type CTicket = Int

-- | Create a new counter initialized to zero.
newCounter :: IO AtomicCounter
newCounter = fmap AtomicCounter $ newIORef 0

-- | Try repeatedly until we successfully increment the counter.
-- incrCounter =

readCounterForCAS :: AtomicCounter -> IO CTicket
readCounterForCAS = readCounter

peekCTicket :: CTicket -> Int
peekCTicket x = x

readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter r) = readIORef r

-- | Make a non-atomic write to the counter.  No memory-barrier.
writeCounter :: AtomicCounter -> Int -> IO ()
writeCounter (AtomicCounter r) !new = writeIORef r new

casCounter :: AtomicCounter -> CTicket -> Int -> IO (Bool, CTicket)
casCounter (AtomicCounter r) oldT !new =

   -- This approach for faking it requires proper equality, it doesn't use pointer
   -- equality at all.  That makes it not a true substitute but useful for some
   -- debugging.
   -- fakeCAS :: Eq a => IORef a -> Ticket a -> a -> IO (Bool,Ticket a)
  
  -- let old = peekTicket oldT
  let old = oldT in 
  atomicModifyIORef r $ \val -> 
{-
    trace ("    DBG: INSIDE ATOMIC MODIFY, ptr eqs found/expected: " ++ 
	   show [ptrEq val old, ptrEq val old, ptrEq val old] ++ 
	   " ptr eq self: " ++ 
	   show [ptrEq val val, ptrEq old old] ++
	   " names: " ++ show (unsafeName old, unsafeName old, unsafeName val, unsafeName val)
	  ) $
-}
    if   (val == old)
    then (new, (True, val))
    else (val, (False,val))
    -- then (new, (True, unsafeCoerce# val))
    -- else (val, (False,unsafeCoerce# val))

{-
{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)

{-# NOINLINE ptrEq #-}
ptrEq :: a -> a -> Bool
ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1

-}
