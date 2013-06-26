{-# LANGUAGE TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

-- | Multiplex multiple queue implementations together using a type family.
module Data.Concurrent.MegaDeque
 (
   -- * Highly parameterized Deque type family.
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
         
  -- ** Aliases enabling more concise Deque type instantiations:
 , S, D, NT, T

  -- ** Aliases for commonly used Deque configurations:
 , Queue, ConcQueue, ConcDeque, WSDeque
 )
  where

import qualified Data.Concurrent.Queue.MichaelScott as MS
import qualified Data.Concurrent.Deque.ChaseLev as CL
-- Fallback implementation:
import qualified Data.Concurrent.Deque.Reference as R

--------------------------------------------------------------------------------

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


------------------------------------------------------------

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

  Finally, this is a closed type family to allow an ordered selection of instances
  with a "fall through" to a reference implementation.

 -}
type family Deque lThreaded rThreaded lDbl rDbl bnd safe elt where

  -- | Work stealing queues are only threadsafe on one end (pop-only) and
  -- double (push/pop) functionality on the other:
  Deque NT T D S Grow Safe elt = CL.ChaseLevDeque elt
  
  -- | This instance classifies the LinkedQueue as being single-ended and fully threadsafe.
  Deque lt rt S S bnd safe elt = MS.LinkedQueue elt

  -- | The reference implementation is the fallback.  It can do anything asked of it,
  -- but it is not nearly as efficient.
  Deque lt rt lDbl rDbl bnd safe elt = R.SimpleDeque elt

------------------------------------------------------------

