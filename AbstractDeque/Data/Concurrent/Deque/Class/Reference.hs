{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

-- | A strawman implementation of concurrent Dequeus.  This
--   implementation is so simple that it also makes a good reference
--   implementation for debugging.
module Data.Concurrent.Deque.Class.Reference 
  ( SimpleDeque(), newQueue ) 
  where

import Prelude hiding (Bounded)
import Data.Concurrent.Deque.Class
import Data.Sequence
import Data.IORef

-- data instance Deque lt rt l r bnd safe elt = DQ (IORef (Seq elt))

type SimpleDeque elt = Deque T T D D Grow Safe elt

newQueue :: IO (SimpleDeque elt)
newQueue = do r <- newIORef empty
	      return (DQ r)


instance DequeClass lt rt l r bnd safe elt where 
  data Deque lt rt l r bnd safe elt = DQ (IORef (Seq elt))

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

------------------------------------------------------------
-- Testing typechecking:

foo :: Deque NT NT S S Bound Safe Int
foo = undefined 

bar = tryPopR foo

foo2 :: Deque lt rt l r bnd safe Int
foo2 = undefined 

bar2 = tryPopR foo

{-
emptydeque :: Deque a 
emptydeque = DQ Seq.empty

takefront (DQ s) = 
  case Seq.viewl s of
    EmptyL  -> (emptydeque, Nothing)
    x :< s' -> (DQ s', Just x)


-}


{-

------------------------------------------------------------
-- Examples / Tests:

-- Test:
data instance Deque NT NT S S Bound Safe elt = DequeL [elt]
foo (DequeL []) = "hello"

bar :: Deque T T D D Grow Safe elt -> elt
bar q = undefined

instance DequeClass (Deque T T S S Grow Safe) where 
  pushL = undefined
  popR  = undefined

instance DequeClass (Deque T T D D Grow Safe) where 
  pushL = undefined
  popR  = undefined
instance PopL       (Deque T T D D Grow Safe) where 
  popL  = undefined
instance PushR      (Deque T T D D Grow Safe) where 
  pushR = undefined

-- The problem here is that there's no way for an implementation to 

test :: (Num elt, PopL d, BoundedPushR d, Bounded d) => d elt -> IO Bool
test x = do popL x; pushR x 3; tryPushR x 3; tryPushL x 3

-}