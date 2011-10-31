{-# LANGUAGE TypeFamilies, CPP, TypeSynonymInstances #-}

-- module Data.DequeFamily where 

import Prelude hiding (Bounded)

#if 0
data ThreadsafeSingleEnd
data ThreadsafeDoubleEnd
data NonthreadsafeSingleEnd
data NonthreadsafeDoubleEnd

data Bound
data Grow

type TS = ThreadsafeSingleEnd
type TD = ThreadsafeDoubleEnd
type S  = NonthreadsafeSingleEnd
type D  = NonthreadsafeDoubleEnd

data Deque left right bnd elt 

-- Could use a class to abstract over both double ended options if
-- doing the 4-param rather than 6-param version:

class DblEnded a where 
instance DblEnded NonthreadsafeDoubleEnd where 
instance DblEnded ThreadsafeDoubleEnd    where


-- Minimally functional Q
newQ :: IO (Deque S S Bound elt)
newQ = undefined

-- Maximally functional Q
newQ2 :: IO (Deque TD TD Grow elt)
newQ2 = undefined

-- Natural case, push left, pop right:
pushL :: Deque a b c etl -> elt -> IO ()
pushL = undefined

---------------------
-- Double ended cases, pop left, push right:

popL :: Deque TD b c etl -> IO elt
popL = undefined
-- ^^ Should the normal popL require thread safety?  Could instead have a
-- separate 'spop' -- a potentially single-threaded pop.


pushR :: DblEnded b => Deque a b c etl -> elt -> IO ()
pushR = undefined

-- Is there anything that would REQUIRE bounded?
-- Yes, tryPush:
tryPushL :: Deque a b Bound etl -> elt -> IO Bool
tryPushL = undefined

class DequeC q where
  data DequeB q :: * -> *

-- data family XList a
-- data instance XList Char = XCons !Char !(XList Char) | XNil 
-- data instance XList () = XListUnit !Int

class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

#else 
----------------------------------------------------------------------------------------------------
--   Formulation 2
----------------------------------------------------------------------------------------------------


-- Drawback in the above formulation: it makes it harder for a
-- function to require threadsafe operation without specifying
-- single/double endings (would require DblEnded class).  Another
-- approach is to simply include six rather than four type arguments
-- to Deque.


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

-- | Aliases for more concise Deque types:
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
-- Natural queue operations that hold for single, 1.5, or double ended modes:
-- Natural case, push left, pop right:
pushL :: Deque lt rt l r  bnd sf elt -> elt -> IO ()
pushL = undefined

popR  :: Deque lt rt l r bnd sf etl -> IO elt
popR = undefined

-- --------------------------------------------------------------------------------
-- -- Double ended cases, pop left, push right:

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



#endif