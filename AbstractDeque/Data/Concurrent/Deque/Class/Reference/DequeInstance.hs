{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Data.Concurrent.Deque.Class.Reference.DequeInstance () where

import Data.Concurrent.Deque.Class
import qualified Data.Concurrent.Deque.Class.Reference as R

-- data instance Deque lt rt l r bnd safe elt = R.SimpleDeque elt
type instance Deque lt rt l r bnd safe elt = R.SimpleDeque elt

-- Illegal type synonym family application:
-- instance DequeClass (Deque lt rt l r bnd safe) where 

instance DequeClass R.SimpleDeque where 
  newQ     = R.newQ
  pushL    = R.pushL
  tryPopR  = R.tryPopR
instance PopL R.SimpleDeque where 
  tryPopL  = R.tryPopL
instance PushR R.SimpleDeque where 
  pushR    = R.pushR
instance BoundedL R.SimpleDeque where 
  tryPushL = R.tryPushL
instance BoundedR R.SimpleDeque where 
  tryPushR = R.tryPushR

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
