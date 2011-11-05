{-# LANGUAGE TypeFamilies #-}

-- | A strawman implementation of concurrent Dequeus.  This
--   implementation is so simple that it also makes a good reference
--   implementation for debugging.
module Data.Concurrent.Deque.Class.Reference 
 (SimpleDeque(..),
  newQ, pushL, pushR, tryPopR, tryPopL, tryPushL, tryPushR)
 where

import Prelude hiding (Bounded)
-- import Data.Concurrent.Deque.Class
import Data.Sequence
import Data.IORef

-- data instance Deque lt rt l r bnd safe elt = DQ 

newtype SimpleDeque elt = DQ (IORef (Seq elt))

-- type SimpleDeque elt = Deque T T D D Grow Safe elt

newQ = do r <- newIORef empty
	  return (DQ r)
pushL (DQ qr) x = atomicModifyIORef qr (\s -> (x <| s, ()))

tryPopR (DQ qr) = atomicModifyIORef qr $ \s -> 
   case viewr s of
     EmptyR  -> (empty, Nothing)
     s' :> x -> (s', Just x)

--   -- This simplistic version simply spins:
--   popR q = do x <- tryPopR q 
-- 	      case x of 
-- 	        Nothing -> popR q
-- 		Just x  -> return x


--   popL q = do x <- tryPopL q 
-- 	      case x of 
-- 	        Nothing -> popL q
-- 		Just x  -> return x

tryPopL (DQ qr) = atomicModifyIORef qr $ \s -> 
  case viewl s of
    EmptyL  -> (empty, Nothing)
    x :< s' -> (s', Just x)

pushR (DQ qr) x = atomicModifyIORef qr (\s -> (s |> x, ()))

tryPushL q v = pushL q v >> return True

tryPushR q v = pushR q v >> return True
