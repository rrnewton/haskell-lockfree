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

-- | The highly-paramewerized type for Deques.
class DequeClass lThreaded rThreaded lDbl rDbl bnd safe elt where
   data Deque    lThreaded rThreaded lDbl rDbl bnd safe elt :: *

   -- Example, work stealing deque: (Deque NT T  D S Grow elt)
   --
   -- When accepting a queue as input to a function you probably never
   -- want to overconstrain by demanding a less-featureful option.
   --
   --   For example, rather than (Deque NT D T S Grow elt)
   --   You would probably want: (Deque nt D T s Grow elt)

   newQ :: IO (Deque lThreaded rThreaded lDbl rDbl bnd safe elt)

   -- ----------------------------------------------------------
   -- * Natural queue operations that hold for all single, 1.5, and double ended modes.

   -- | Natural push: push left
   pushL    :: Deque lThreaded rThreaded lDbl rDbl bnd safe elt -> elt -> IO ()

   -- | Natural pop: pop right.
   tryPopR  :: Deque lThreaded rThreaded lDbl rDbl bnd safe elt -> IO (Maybe elt)

   -- ----------------------------------------------------------
   -- * The "unnatural" double ended cases: pop left, push right.

   -- | PopL is not the native operation for the left end, so it requires
   --   that the left end be a "Double", but places no other requirements
   --   on the input queue.
   -- 
   --   The implementation is requiredy to block or spin until an element is available.
   tryPopL :: Deque lThreaded rThreaded DoubleEnd rDbl bnd safe elt -> IO (Maybe elt)


   -- -- Should the normal popL require thread safety?  Here's a potentially single-threaded pop:
   -- spopL :: DblEnded a => Deque a b  c elt -> elt -> IO ()
   -- spopL = undefined

   -- | Pushing is not the native operation for the right end, so it requires
   --   that end be a "Double".
   pushR :: Deque lThreaded rThreaded lDbl DoubleEnd bnd safe elt -> elt -> IO ()

   -- ------------------------------------------------------------
   -- * Operations on bounded queues.

   -- tryPush REQUIRES bounded:
   tryPushL :: Deque lThreaded rThreaded lDbl rDbl      Bound safe elt -> elt -> IO Bool
   tryPushR :: Deque lThreaded rThreaded lDbl DoubleEnd Bound safe elt -> elt -> IO Bool

