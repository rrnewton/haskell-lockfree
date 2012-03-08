{-# LANGUAGE TypeFamilies, CPP, TypeSynonymInstances, MultiParamTypeClasses,
    FlexibleInstances, EmptyDataDecls  #-}

#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif

{- |
   An abstract, parameterizable interface for queues.  

   This interface includes a non-associated type family for Deques
   plus separate type classes encapsulating the Deque operations.
   This design strives to hide the extra phantom-type parameters from
   the Class constraints and therefore from the type signatures of
   client code.

-} 
module Data.Concurrent.Deque.Class
 (
  -- * Highly parameterized Deque type(s)
   Deque
  -- ** The choices that select a queue-variant.
  -- *** Choice #1 -- thread safety.
 , Threadsafe, Nonthreadsafe
  -- *** Choice #2 -- double or single functionality on an end.
 , SingleEnd, DoubleEnd
  -- *** Choice #3 -- bounded or growing queues:
 , Bound, Grow
  -- *** Choice #4 -- duplication of elements.
 , Safe, Dup
  -- ** Aliases enabling more concise Deque types:
 , S, D, NT, T

  -- ** Aliases for commonly used Deque configurations:
 , Queue, ConcQueue, ConcDeque, WSDeque

  -- * Classes containing Deque operations
 , DequeClass(..)

  -- * Auxilary type classes.  

  -- | In spite of hiding the extra phantom type
  --  parameters in the DequeClass, we wish to retain the ability for
  --  clients to constrain the set of implementations they work with
  --  **statically**.

  -- ** The \"unnatural\" double ended cases: pop left, push right.
 , PopL(..), PushR(..)
  -- ** Operations that only make sense for bounded queues.
 , BoundedL(..), BoundedR(..)
)


   where

import Prelude hiding (Bounded)

{- | 

 A family of Deques implementations.  A concrete Deque implementation
 is selected based on the (phantom) type parameters, which encode
 several choices.

 For example, a work stealing deque is threadsafe only on one end and
 supports push/pop on one end (and pop-only) on the other:

  >> (Deque NT T  D S Grow elt)

 Note, however, that the above example is overconstraining in many
 situations.  It demands an implementation which is NOT threadsafe on
 one end and does NOT support push on one end, whereas both these
 features would not hurt, if present.

 Thus when accepting a queue as input to a function you probably never
 want to overconstrain by demanding a less-featureful option.

   For example, rather than @(Deque NT D T S Grow elt)@
   You would probably want: @(Deque nt D T s Grow elt)@

 -}
-- data family Deque lThreaded rThreaded lDbl rDbl bnd safe elt 
type family Deque lThreaded rThreaded lDbl rDbl bnd safe elt 

-- | Haskell IO threads ("Control.Concurrent") may concurrently access
--   this end of the queue.  Note that this attribute is set
--   separately for the left and right ends.
data Threadsafe
-- | Only one thread at a time may access this end of the queue.
data Nonthreadsafe

-- | This end of the queue provides push-only (left) or pop-only
--   (right) functionality. Thus a 'SingleEnd' / 'SingleEnd' combination
--   is what is commonly referred to as a /single ended queue/, whereas
--   'DoubleEnd' / 'DoubleEnd' is 
--   a /double ended queue/.  Heterogeneous combinations are sometimes
--   colloquially referred to as \"1.5 ended queues\".
data SingleEnd
-- | This end of the queue supports both push and pop.
data DoubleEnd

-- | The queue has bounded capacity.
data Bound
-- | The queue can grow as elements are added.
data Grow

-- | The queue will not duplicate elements.
data Safe  
-- | Pop operations may possibly duplicate elements.  Hopefully with low probability!
data Dup   

-- Possible #5:
-- data Lossy -- I know of no algorithm which would motivate having a Lossy mode.

----------------------------------------

type T  = Threadsafe
type NT = Nonthreadsafe
type S  = SingleEnd
type D  = DoubleEnd

-- | A traditional single-threaded, single-ended queue.
type Queue a = Deque Nonthreadsafe Nonthreadsafe SingleEnd SingleEnd Grow Safe a
-- | A concurrent queue.
type ConcQueue a = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe a
-- | A concurrent deque.
type ConcDeque a = Deque Threadsafe Threadsafe DoubleEnd DoubleEnd Grow Safe a
-- | Work-stealing deques (1.5 ended).  Typically the worker pushes
--   and pops its own queue (left) whereas thieves only pop (right).
type WSDeque a = Deque Nonthreadsafe Threadsafe DoubleEnd SingleEnd Grow Safe a

--------------------------------------------------------------------------------

-- | Class encompassing the basic queue operations that hold for all
--   single, 1.5, and double ended modes.  We arbitrarily call the
--   ends \"left\" and \"right\" and choose the natural operations to be
--   pushing on the left and popping on the right.
class DequeClass d where
   -- | Create a new deque.  Most appropriate for unbounded deques.
   --   If bounded, the size is unspecified.
   newQ  :: IO (d elt)

#ifdef DEFAULT_SIGNATURES
#warning "Providing default binding and signature for newQ..."
   default newQ :: BoundedL d => IO (d elt)
   newQ = newBoundedQ 256
#endif

   -- | Is the queue currently empty?  Beware that this can be a highly transient state.
   nullQ :: d elt -> IO Bool

   -- | Natural push: push onto the left end of the deque.
   pushL :: d elt -> elt -> IO ()
   -- | Natural pop: pop from the right end of the deque.
   tryPopR  :: d elt -> IO (Maybe elt)

   -- TODO: Consider adding a peek operation?

   -- TODO: It would also be possible to include blocking/spinning pops.
   -- But maybe those should go in separate type classes...

class DequeClass d => PopL d where 
   -- | PopL is not the native operation for the left end, so it requires
   --   that the left end be a 'DoubleEnd', but places no other requirements
   --   on the input queue.
   -- 
   tryPopL  :: d elt -> IO (Maybe elt)

class DequeClass d => PushR d where 
   -- | Pushing is not the native operation for the right end, so it requires
   --   that end be a 'DoubleEnd'.
   pushR :: d elt -> elt -> IO ()

class DequeClass d => BoundedL d where 
   -- | Create a new, bounded deque with a specified capacity.
   newBoundedQ :: Int -> IO (d elt)
   -- | For a bounded deque, pushing may fail if the deque is full.
   tryPushL :: d elt -> elt -> IO Bool

class PushR d => BoundedR d where 
   -- | For a bounded deque, pushing may fail if the deque is full.
   tryPushR :: d elt -> elt -> IO Bool

