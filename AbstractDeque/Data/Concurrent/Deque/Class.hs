{-# LANGUAGE TypeFamilies, CPP, TypeSynonymInstances #-}

-- module Data.DequeFamily where 

import Prelude hiding (Bounded)

----------------------------------------------------------------------------------------------------

-- | The highly-paramewerized type for Deques.
data Deque lThreaded rThreaded lDbl rDbl bnd safe elt

-- Example, work stealing deque: (Deque NT D T S Grow elt)
--                       versus: (Deque D TS Grow elt)
--
-- But really you never want to overconsrain by forcing the
-- less-functional option when accepting a queue as input to a
-- function.
--   So rather than (Deque NT D T S Grow elt)
--   You would probably want: (Deque nt D T s Grow elt)
--     Could also nest this I guess:  (Deque (D nt) (S T) Grow elt)


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
-- Example queue creation functions:

-- Minimally functional Q
newQ :: IO (Deque NT NT S S Bound Safe elt)
newQ = undefined

-- Maximally functional Q
newQ2 :: IO (Deque T T D D Grow Safe elt)
newQ2 = undefined

--------------------------------------------------------------------------------
-- * Natural queue operations that hold for all single, 1.5, and double ended modes.

-- | Natural push: push left
pushL :: Deque lt rt l r  bnd sf elt -> elt -> IO ()
pushL = undefined

-- | Natural pop: pop right.
popR  :: Deque lt rt l r bnd sf etl -> IO elt
popR = undefined

-- --------------------------------------------------------------------------------
-- * Double ended cases, pop left, push right:

-- | PopL is not the native operation for the left end, so it requires
--   that the left end be a "Double", but places no other requirements
--   on the input queue.
-- 
--   The implementation is requiredy to block or spin until an element is available.
popL :: Deque lt rt DoubleEnd r bnd sf elt -> IO elt
popL = undefined

-- -- Should the normal popL require thread safety?  Here's a potentially single-threaded pop:
-- spopL :: DblEnded a => Deque a b  c etl -> elt -> IO ()
-- spopL = undefined

-- | Pushing is not the native operation for the right end, so it requires
--   that end be a "Double".
pushR :: Deque lt rt l DoubleEnd bnd sf elt -> elt -> IO ()
pushR = undefined


-- --------------------------------------------------------------------------------
-- -- We only need to "tryPush" 

-- -- Is there anything that would REQUIRE bounded?
-- tryPushL :: Deque a b Bound etl -> elt -> IO Bool
-- tryPushL = undefined

