{-# LANGUAGE TypeFamilies, CPP #-}

{-| 
  A strawman implementation of concurrent Dequeues.  This
  implementation is so simple that it also makes a good reference
  implementation for debugging.

  The queue representation is simply an IORef containing a Data.Sequence.

  Also see "Data.Concurrent.Deque.Reference.DequeInstance".
  By convention a module of this name is also provided.

-}

module Data.Concurrent.Deque.Reference 
 (SimpleDeque(..),
  newQ, nullQ, newBoundedQ, pushL, pushR, tryPopR, tryPopL, tryPushL, tryPushR
 )
 where

import Prelude hiding (length)
import qualified Data.Concurrent.Deque.Class as C
import Data.Sequence
import Data.IORef
#ifdef USE_CAS
#warning "abstract-deque: reference implementation using CAS..."
import Data.CAS (atomicModifyIORefCAS)
-- Toggle these and compare performance:
modify = atomicModifyIORefCAS
#else
modify = atomicModifyIORef
#endif
{-# INLINE modify #-}

-- | Stores a size bound (if any) as well as a mutable Seq.
data SimpleDeque elt = DQ {-# UNPACK #-} !Int !(IORef (Seq elt))


newQ = do r <- newIORef empty
	  return (DQ 0 r)

newBoundedQ lim = 
  do r <- newIORef empty
     return (DQ lim r)

pushL (DQ 0 qr) x = modify qr (\s -> (x <| s, ()))
pushL (DQ n _) _ = error$ "should not call pushL on Deque with size bound "++ show n

tryPopR (DQ _ qr) = modify qr $ \s -> 
   case viewr s of
     EmptyR  -> (empty, Nothing)
     s' :> x -> (s', Just x)

nullQ :: SimpleDeque elt -> IO Bool
nullQ (DQ _ qr) = 
  do s <- readIORef qr
     case viewr s of 
       EmptyR -> return True
       _ :> _ -> return False

--   -- This simplistic version simply spins:
--   popR q = do x <- tryPopR q 
-- 	      case x of 
-- 	        Nothing -> popR q
-- 		Just x  -> return x

--   popL q = do x <- tryPopL q 
-- 	      case x of 
-- 	        Nothing -> popL q
-- 		Just x  -> return x

tryPopL (DQ _ qr) = modify qr $ \s -> 
  case viewl s of
    EmptyL  -> (empty, Nothing)
    x :< s' -> (s', Just x)

pushR (DQ 0 qr) x = modify qr (\s -> (s |> x, ()))
pushR (DQ n _) _ = error$ "should not call pushR on Deque with size bound "++ show n

tryPushL q@(DQ 0 _) v = pushL q v >> return True
tryPushL (DQ lim qr) v = 
  modify qr $ \s -> 
     if length s == lim
     then (s, False)
     else (v <| s, True)

tryPushR q@(DQ 0 _) v = pushR q v >> return True
tryPushR (DQ lim qr) v = 
  modify qr $ \s -> 
     if length s == lim
     then (s, False)
     else (s |> v, True)

--------------------------------------------------------------------------------
--   Instances
--------------------------------------------------------------------------------

instance C.DequeClass SimpleDeque where 
  newQ     = newQ
  nullQ    = nullQ
  pushL    = pushL
  tryPopR  = tryPopR
instance C.PopL SimpleDeque where 
  tryPopL  = tryPopL
instance C.PushR SimpleDeque where 
  pushR    = pushR

instance C.BoundedL SimpleDeque where 
  tryPushL    = tryPushL
  newBoundedQ = newBoundedQ

instance C.BoundedR SimpleDeque where 
  tryPushR = tryPushR

