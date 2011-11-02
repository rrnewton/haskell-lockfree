{-# LANGUAGE TypeFamilies, CPP, TypeSynonymInstances, MultiParamTypeClasses,
    FlexibleInstances, EmptyDataDecls #-}

module Data.Concurrent.Deque.Class where 

import Prelude hiding (Bounded)

----------------------------------------------------------------------------------------------------

-- * The choices that select a queue-variant.

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
data Safe  -- | Will not duplicate elements.
data Dup   -- | Possibly duplicating elements on pop.
-- data Lossy -- I know of no algorithm which would motivate having a Lossy mode.

------------------------------------------------------------

-- * Aliases for more concise Deque types:
type T  = Threadsafe
type NT = Nonthreadsafe
type S  = SingleEnd
type D  = DoubleEnd

--------------------------------------------------------------------------------

-- [Presently considering two formulations for the type family.  The
-- first variant separates the data family from the type class and
-- thereby hides the (excessive) six phantom-type parameters, at the
-- expense of introducing a bunch of type auxilary type classes.]

#if 1
    
data family Deque lThreaded rThreaded lDbl rDbl bnd safe elt 

-- Example, work stealing deque: (Deque NT T  D S Grow elt)
--
-- When accepting a queue as input to a function you probably never
-- want to overconstrain by demanding a less-featureful option.
--
--   For example, rather than (Deque NT D T S Grow elt)
--   You would probably want: (Deque nt D T s Grow elt)

class DequeClass d where
   newQ  :: IO (d elt)
   pushL :: d elt -> elt -> IO ()
   tryPopR  :: d elt -> IO (Maybe elt)

-- In spite of hiding the extra phantom type parameters in the
--  DequeClass, we wish to retain the ability for clients to constrain
--  the set of implementations they work with **statically**.
class DequeClass d => PopL d where 
   tryPopL  :: d elt -> IO (Maybe elt)
class DequeClass d => PushR d where 
   pushR :: d elt -> elt -> IO ()
class DequeClass d => BoundedL d where 
--   newBoundedQ :: Int -> IO (d elt) -- Presently leaving it up to the implementation what range of Q sizes are accepted.
   tryPushL :: d elt -> elt -> IO Bool
class PushR d => BoundedR d where 
   tryPushR :: d elt -> elt -> IO Bool

#else

-- | The highly-paramewerized type for Deques.
class DequeLike lThreaded rThreaded lDbl rDbl bnd safe elt where
   data Deque lThreaded rThreaded lDbl rDbl bnd safe elt :: *


   -- ----------------------------------------------------------
   -- * Natural queue operations that hold for all single, 1.5, and double ended modes.

   -- | Natural push: push left
   pushL :: Deque lt rt l r  bnd sf elt -> elt -> IO ()

   -- | Natural pop: pop right.
   popR  :: Deque lt rt l r bnd sf elt -> IO elt

   -- ----------------------------------------------------------
   -- * The "unnatural" double ended cases: pop left, push right.

   -- | PopL is not the native operation for the left end, so it requires
   --   that the left end be a "Double", but places no other requirements
   --   on the input queue.
   -- 
   --   The implementation is requiredy to block or spin until an element is available.
   popL :: Deque lt rt DoubleEnd r bnd sf elt -> IO elt
   popL = undefined

   -- -- Should the normal popL require thread safety?  Here's a potentially single-threaded pop:
   -- spopL :: DblEnded a => Deque a b  c elt -> elt -> IO ()
   -- spopL = undefined

   -- | Pushing is not the native operation for the right end, so it requires
   --   that end be a "Double".
   pushR :: Deque lt rt l DoubleEnd bnd sf elt -> elt -> IO ()
   pushR = undefined

   -- ------------------------------------------------------------
   -- * Operations on bounded queues.

   -- tryPush REQUIRES bounded:
   tryPushL :: Deque lt rt l  r        Bound safe elt -> elt -> IO Bool
   tryPushR :: Deque lt rt l DoubleEnd Bound safe elt -> elt -> IO Bool


#endif

