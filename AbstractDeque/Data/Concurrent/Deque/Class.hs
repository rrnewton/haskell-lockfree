{-# LANGUAGE TypeFamilies, CPP, TypeSynonymInstances, MultiParamTypeClasses,
    FlexibleInstances, EmptyDataDecls, DefaultSignatures #-}

{- |
   An abstract, parameterizable interface for queues.  

   The basic design involves a non-associated type family + separate
   type classes.  This design strives to hide the extra phantom-type
   parameters from the Class constraints and therefore from the type
   signatures of client code.
 -}
module Data.Concurrent.Deque.Class where 

import Prelude hiding (Bounded)


--------------------------------------------------------------------------------
-- * Highly parameterized Deque type(s)
--------------------------------------------------------------------------------

-- | A Deque is selected based on several choices.
-- 
-- Example, a work stealing deque: 
-- 
--  >> (Deque NT T  D S Grow elt)
--
-- Note: when accepting a queue as input to a function you probably
-- never want to overconstrain by demanding a less-featureful option.
--
--   For example, rather than (Deque NT D T S Grow elt)
--   You would probably want: (Deque nt D T s Grow elt)
data family Deque lThreaded rThreaded lDbl rDbl bnd safe elt 

--------------------------------------------------------------------------------
-- * The choices that select a queue-variant.
--------------------------------------------------------------------------------

-- | Choice #1 -- thread saftety.
data Threadsafe
data Nonthreadsafe

-- | Choice #2 -- double or or single functionality (push/pop) on EACH end.
data SingleEnd
data DoubleEnd

-- | Choice #3 -- bounded or growing queues:
data Bound
data Grow

-- | Choice #4 -- duplication of elements.
data Safe  --  Will not duplicate elements.
data Dup   --  Possibly duplicating elements on pop.

-- Possible #5:
-- data Lossy -- I know of no algorithm which would motivate having a Lossy mode.

------------------------------------------------------------
-- * Aliases for more concise Deque types:

type T  = Threadsafe
type NT = Nonthreadsafe
type S  = SingleEnd
type D  = DoubleEnd

--------------------------------------------------------------------------------
-- * Classes containing Deque operations
--------------------------------------------------------------------------------

class DequeClass d where
   newQ  :: IO (d elt)

--   default newQ :: BoundedL d => IO (d elt)
--   newQ = newBoundedQ 256

   -- ----------------------------------------------------------
   -- * Natural queue operations that hold for all single, 1.5, and double ended modes.
   -- | Natural push: push left
   pushL :: d elt -> elt -> IO ()
   tryPopR  :: d elt -> IO (Maybe elt)

-- --------------------------------------------------------------------------------
-- * Auxilary type classes.  In spite of hiding the extra phantom type
--  parameters in the DequeClass, we wish to retain the ability for
--  clients to constrain the set of implementations they work with
--  **statically**.

-- ----------------------------------------------------------
-- * The "unnatural" double ended cases: pop left, push right.

class DequeClass d => PopL d where 
   -- | PopL is not the native operation for the left end, so it requires
   --   that the left end be a "Double", but places no other requirements
   --   on the input queue.
   -- 
   tryPopL  :: d elt -> IO (Maybe elt)

class DequeClass d => PushR d where 
   -- | Pushing is not the native operation for the right end, so it requires
   --   that end be a "Double".
   pushR :: d elt -> elt -> IO ()

class DequeClass d => BoundedL d where 
--   newBoundedQ :: Int -> IO (d elt) -- Presently leaving it up to the implementation what range of Q sizes are accepted.
   newBoundedQ :: Int -> IO (d elt)
   tryPushL :: d elt -> elt -> IO Bool

class PushR d => BoundedR d where 
   tryPushR :: d elt -> elt -> IO Bool

