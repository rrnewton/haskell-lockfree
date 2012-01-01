{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FunctionalDependencies,
    FlexibleContexts, ScopedTypeVariables, CPP #-}

-- Experimenting with sum and product types in an EDSL.

-- Tuple / untuple approach.... that's Accelerate's aproach.


import Data.IORef
import qualified Data.CAS as CAS

-- Let's imagine that we could have a single data definition that coud
-- be used both for generating Haskell data structures, and for
-- external (say, C++) data structures.  

-- Overloading is unlikely to be sufficient for this.  It would
-- require Template Haskell, as follows:

#if 0
$[hdats|
 -- Here's our abstract data definition.
 data LinkedQueue a = LQ 
    { head :: IORef (Pair a)
    , tail :: IORef (Pair a)
    }
 data Pair a = Null | Cons a (Ref (Pair a))
 ]

#else
-- The above generates... :
------------------------------------------------------------
-- First, generic operations for the above datatype:

class DatM e m => PairOps e m where 
  mkNull :: e (Pair a)
  mkCons :: e a -> e (IORef (Pair a)) -> e (Pair a)

  -- "case", like "if", cannot be overloaded.  Hence reified case
  -- expressions:
  casePair :: PairOps e m
	   => e (Pair a) 
	   -> r                                 -- Null case RHS
	   -> (e a -> e (IORef (Pair a)) -> r)  -- Cons case RHS
	   -> r

instance PairOps FakeExp IO where 
  -- mkNull :: FakeExp (Pair a)
  mkNull = FakeExp Null
  -- mkCons :: FakeExp a -> FakeExp (IORef (Pair a)) -> FakeExp (Pair a)
  mkCons (FakeExp hd) (FakeExp ref) = FakeExp (Cons hd ref)
-- For uniformity we should probably access LinkedQueue via this:
-- caseLinkedQueue (FakeExp q) f = f (head q) (tail q)

  casePair (FakeExp x) f g = 
    case x of 
      Null -> f 
      Cons a b -> g (FakeExp a) (FakeExp b)

-- For uniformity we should probably access LinkedQueue via this:
-- caseLinkedQueue (FakeExp q) f = f (head q) (tail q)

------------------------------------------------------------
-- Second, a Haskell-specific instantiation:

data LinkedQueue a = LQ 
    { head :: FakeExp (IORef (Pair a))
    , tail :: FakeExp (IORef (Pair a))
    }
data Pair a = Null | Cons a (IORef (Pair a))

newtype FakeExp a = FakeExp a 
  deriving Show 

--instance DatM IORef IO where 
instance DatM FakeExp IO where 
  newRef   (FakeExp x) = newIORef  x     >>= (return . FakeExp)
  readRef  (FakeExp x) = readIORef x     >>= (return . FakeExp)
  writeRef (FakeExp x) (FakeExp v) = writeIORef x v
  error_ (FakeExp s) = error s
  casRef (FakeExp r) (FakeExp o) (FakeExp n) =
    do (b,v) <- CAS.casIORef r o n
       return (FakeExp b, FakeExp v)

  ptrEq (FakeExp a) (FakeExp b) = FakeExp (CAS.ptrEq a b)
  not_  (FakeExp x) = FakeExp (not x)
  if_   (FakeExp x) th el = (if x then th else el)
  true     = FakeExp True
  false    = FakeExp False

  call fn x = fn x

  str = FakeExp


instance Num a => Num (FakeExp a) where
  (FakeExp a) + (FakeExp b) = FakeExp (a+b)
  (FakeExp a) - (FakeExp b) = FakeExp (a-b)
  (FakeExp a) * (FakeExp b) = FakeExp (a*b)
  abs           (FakeExp a) = FakeExp (abs a)
  signum        (FakeExp a) = FakeExp (signum a)
  fromInteger   i           = FakeExp (fromInteger i)
#endif
-- End Generated Code.
-- ================================================================================

-- class DatM e m | e -> m where 
-- class DatM e m | m -> e where 
class Monad m => DatM e m | m -> e, e -> m where  -- Bi-directional functional dependency.
--  type Bl -- Booleans
  newRef   :: e a -> m (e (IORef a))
  readRef  :: e (IORef a) -> m (e a)
  writeRef :: e (IORef a) -> e a -> m ()
--  casRef   :: e a -> a -> a -> IO (Bool,a)
  casRef   :: e (IORef a) -> e a -> e a -> m (e Bool, e a)
  -- The trick here would be to pack untupling into the calling convention...
--  call     :: Callable a => (a -> m b) -> m b
  call     :: (e a -> m b) -> e a -> m b
  error_   :: e String -> m a

  ptrEq    :: e a -> e a -> e Bool
  not_     :: e Bool -> e Bool
  if_      :: e Bool -> a -> a -> a
  true     :: e Bool
  false    :: e Bool

  -- We can get rid of this simply with "OverloadedStrings":
  str      :: String -> e String

-- untuple :: DatM e m => LinkedQueue a -> ()
untuple = undefined


class Callable a where 
-- Can we do the analog of "untupling" for sum types here?
-- And can we do it implicitly at the call sites??
--
-- foo :: Pair -> EmitC (Exp Int)
-- foo Null = return 0
-- foo (Cons _ _) = return 1 

-- bar = call foo (v::Pair)
-- baz = call foo (v::Exp Pair) -- This one needs to "untuple".

-- ==================================================================================================
-- Here is a generic data structure operation.
-- ==================================================================================================

-- | Push a new element onto the queue.  Because the queue can grow,
--   this alway succeeds.

pushL :: PairOps e m => e (LinkedQueue a) -> e a -> m ()
pushL lq val = do

--   let (LQ headPtr tailPtr) = untuple lq
   r <- newRef mkNull
   let newp = mkCons val r   -- Create the new cell that stores val.
   tail <- call loop newp 
   -- After the loop, enqueue is done.  Try to swing the tail.
   -- If we fail, that is ok.  Whoever came in after us deserves it.
   casRef tailPtr tail newp
   return ()
 where
  (headPtr,tailPtr) = untuple lq  -- Magic
--  LQ headPtr tailPtr = untuple lq  -- Magic

  loop newp = do 
   tail <- readRef tailPtr -- Reread the tailptr from the queue structure.
   casePair tail 
     (error_ (str "push: LinkedQueue invariants broken.  Internal error."))
     (\ _ next -> do
	next' <- readRef next
	-- The algorithm rereads tailPtr here to make sure it is still good.
	-- 
	-- There's a possibility for an infinite loop here with StableName based ptrEq.
	-- (And at one point I observerd such an infinite loop.)
	-- But with one based on reallyUnsafePtrEquality# we should be ok.
	tail' <- readRef tailPtr
--        b <- refEq tailPtr  -- Could only allow ptr comparison for references...
        if_ (not_ (ptrEq tail tail')) 
          (call loop newp)
	  (casePair next' 
	   -- We skip that simply because comparing pointers would require StableNames.
	   (do (b,newtail) <- casRef next next' newp
	       if_ b (return tail)
		     (call loop newp))
	   (\ _ _ -> do 
	      -- We try to bump the tail in this case, but if we don't someone else will.
	      casRef next next' newp
	      return tail))
     )

{-


-- | Attempt to pop an element from the queue if one is available.
--   tryPop will always return promptly, but will return 'Nothing' if
--   the queue is empty.
tryPopR ::  LinkedQueue a -> IO (Maybe a)
tryPopR (LQ headPtr tailPtr) = loop
 where 
  loop = do 
    head <- readIORef headPtr
    tail <- readIORef tailPtr
    case head of 
      Null -> error "tryPopR: LinkedQueue invariants broken.  Internal error."
      Cons _ next -> do
        next' <- readIORef next
        -- As with push, double-check our information is up-to-date. (head,tail,next consistent)
        head' <- readIORef headPtr
        if not (ptrEq head head') then loop else do 
	  -- Is queue empty or tail falling behind?:
          if ptrEq head tail then do 
	    case next' of -- Is queue empty?
              Null -> return Nothing -- Queue is empty, couldn't dequeue
	      Cons _ _ -> do
  	        -- Tail is falling behind.  Try to advance it:
	        casIORef tailPtr tail next'
		loop 
           
	   else do -- head /= tail
	      -- No need to deal with Tail.  Read value before CAS.
	      -- Otherwise, another dequeue might free the next node
	      case next' of 
--	        Null -> error "tryPop: Internal error.  Next should not be null if head/=tail."
	        Null -> loop 
		Cons value _ -> do 
                  -- Try to swing Head to the next node
		  (b,_) <- casIORef headPtr head next'
		  if b then return (Just value) -- Dequeue done; exit loop.
		       else loop   
          
-- | Create a new queue.
newQ :: IO (LinkedQueue a)
newQ = do 
  r <- newIORef Null
  let newp = Cons (error "LinkedQueue: Used uninitialized magic value.") r
  hd <- newIORef newp
  tl <- newIORef newp
  return (LQ hd tl)

-- | Is the queue currently empty?  Beware that this can be a highly transient state.
nullQ :: LinkedQueue a -> IO Bool
nullQ (LQ headPtr tailPtr) = do 
    head <- readIORef headPtr
    tail <- readIORef tailPtr
    return (ptrEq head tail)

-}

--------------------------------------------------------------------------------
--   Instance(s) of abstract deque interface
--------------------------------------------------------------------------------

-- instance C.DequeClass LinkedQueue where 
--   newQ    = newQ
--   nullQ   = nullQ
--   pushL   = pushL
--   tryPopR = tryPopR

--------------------------------------------------------------------------------
--   Notes
--------------------------------------------------------------------------------


t0 q = do
--  LQ 
  pushL q (str "hi")
--  Just x <- tryPopL q 
--  assertEqual "test_ws_triv1" x "hi"
