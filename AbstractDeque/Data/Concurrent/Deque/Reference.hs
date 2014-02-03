{-# LANGUAGE TypeFamilies, CPP, BangPatterns #-}

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
  newQ, nullQ, newBoundedQ, pushL, pushR, tryPopR, tryPopL, tryPushL, tryPushR,
  
  _is_using_CAS -- Internal
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
_is_using_CAS = True
#else
modify = atomicModifyIORef
_is_using_CAS = False
#endif

{-# INLINE modify #-}
modify :: IORef a -> (a -> (a, b)) -> IO b
_is_using_CAS :: Bool


-- | Stores a size bound (if any) as well as a mutable Seq.
data SimpleDeque elt = DQ {-# UNPACK #-} !Int !(IORef (Seq elt))


newQ :: IO (SimpleDeque elt)
newQ = do r <- newIORef empty
	  return $! DQ 0 r

newBoundedQ :: Int -> IO (SimpleDeque elt)
newBoundedQ lim = 
  do r <- newIORef empty
     return $! DQ lim r

pushL :: SimpleDeque t -> t -> IO ()
pushL (DQ 0 qr) !x = do 
   () <- modify qr addleft
   return ()
 where 
   -- Here we are very strict to avoid stack leaks.
   addleft !s = extended `seq` pair
    where extended = x <| s 
          pair = (extended, ())
pushL (DQ n _) _ = error$ "should not call pushL on Deque with size bound "++ show n

tryPopR :: SimpleDeque a -> IO (Maybe a)
tryPopR (DQ _ qr) = modify qr $ \ s -> 
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

tryPopL :: SimpleDeque a -> IO (Maybe a)
tryPopL (DQ _ qr) = modify qr $ \s -> 
  case viewl s of
    EmptyL  -> (empty, Nothing)
    x :< s' -> (s', Just x)

pushR :: SimpleDeque t -> t -> IO ()
pushR (DQ 0 qr) x = modify qr (\s -> (s |> x, ()))
pushR (DQ n _) _ = error$ "should not call pushR on Deque with size bound "++ show n

tryPushL :: SimpleDeque a -> a -> IO Bool
tryPushL q@(DQ 0 _) v = pushL q v >> return True
tryPushL (DQ lim qr) v = 
  modify qr $ \s -> 
     if length s == lim
     then (s, False)
     else (v <| s, True)

tryPushR :: SimpleDeque a -> a -> IO Bool
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
  leftThreadSafe _ = True
  rightThreadSafe _ = True
instance C.PopL SimpleDeque where 
  tryPopL  = tryPopL
instance C.PushR SimpleDeque where 
  pushR    = pushR

instance C.BoundedL SimpleDeque where 
  tryPushL    = tryPushL
  newBoundedQ = newBoundedQ

instance C.BoundedR SimpleDeque where 
  tryPushR = tryPushR

