{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- TypeFamilies, FlexibleInstances

-- | Michael and Scott lock-free, single-ended queues.
-- 
-- This is a straightforward implementation of classic Michael & Scott Queues.
-- Pseudocode for this algorithm can be found here:
-- 
--   <http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html>

-- Uncomment this if desired.  Needs more testing:
-- #define RECHECK_ASSUMPTIONS

module Data.Concurrent.Queue.MichaelScott
 (
   -- The convention here is to directly provide the concrete
   -- operations as well as providing the typeclass instances.
   LinkedQueue(), newQ, nullQ, pushL, tryPopR, 
 )
  where

import Data.IORef (readIORef, IORef, newIORef)
import System.IO (stderr)
import Data.ByteString.Char8 (hPutStrLn, pack)

import GHC.Prim -- (MutVar#, RealWorld, sameMutVar#, newMutVar#, casMutVar#, readMutVar#)
import GHC.IO (IO(IO))

import qualified Data.Concurrent.Deque.Class as C
-- NOTE: you can switch which CAS implementation is used here:
--------------------------------------------------------------
-- import Data.CAS (casIORef, ptrEq)
-- -- import Data.CAS.Internal.Fake (casIORef, ptrEq)
-- -- #warning "Using fake CAS"
-- -- import Data.CAS.Internal.Native (casIORef, ptrEq)
-- -- #warning "Using NATIVE CAS"
--------------------------------------------------------------
-- import Data.Atomics(casMutVar, readMutVarForCAS)
import Data.Atomics(casMutVar, readMutVarForCAS, Ticket)
import Data.Atomics.Internal(casMutVarTicketed#, readForCAS#, Ticket#)


-- Considering using the Queue class definition:
-- import Data.MQueue.Class

data LinkedQueue a = LQ 
    { head :: MutVar# RealWorld (Pair a)
    , tail :: MutVar# RealWorld (Pair a)
    }

data Pair a = Null | Cons a (MutVar# RealWorld (Pair a))

-- Only checks that the node type is the same and in the case of a Cons Pair checks that
-- the IORefs are pointer-equal. This suffices to check equality since IORefs are never used in different 
-- Pair values.
pairEq :: Pair a -> Pair a -> Bool
pairEq Null       Null        = True
pairEq (Cons _ r) (Cons _ r') = sameMutVar# r r'
pairEq _          _           = False


-- | Push a new element onto the queue.  Because the queue can grow,
--   this always succeeds.
pushL :: forall a . LinkedQueue a -> a  -> IO ()
pushL q@(LQ headPtr tailPtr) val = IO $ \ st1 ->
  case newMutVar# Null st1 of
    (# st2, mv #) ->
     let newp = Cons val mv in -- Create the new cell that stores val.
     case loop st2 newp of
       (# st3, tailTicket, tail #) -> 
        -- After the loop, enqueue is done.  Try to swing the tail.
        -- If we fail, that is ok.  Whoever came in after us deserves it.         
--        case casMutVar# tailPtr tailTicket newp st3 of
        -- (WAIT, but what if we fail due to GC!? [2013.04.23])
        case casMutVar# tailPtr tail newp st3 of
          (# st4, flag, res #) -> (# st4, () #)
 where

  loop :: State# RealWorld -> Pair a -> (# State# RealWorld, Ticket#, Pair a #)
  loop s1 newp = 
   case readForCAS# tailPtr s1 of -- [Re]read the tailptr from the queue structure.
     (# s2, tailTicket#, tail #) ->
      case tail of
       -- The head and tail pointers should never themselves be NULL:
       Null -> error "push: LinkedQueue invariants broken.  Internal error."
       Cons _ nextMV ->
        case readForCAS# nextMV s2 of
         (# s3, nextTicket#, next #) ->
             {-
             -- Optimization: The algorithm can reread tailPtr here to make sure it is still good:
             #ifdef RECHECK_ASSUMPTIONS
              -- There's a possibility for an infinite loop here with StableName based ptrEq.
              -- (And at one point I observed such an infinite loop.)
              -- But with one based on reallyUnsafePtrEquality# we should be ok.
                     tail' <- readIORef tailPtr   -- ANDREAS: used atomicModifyIORef here
                     if not (pairEq tail tail') then loop newp 
                      else case next of 
             #else
                     case next of 
             #endif
             -}
           case next of
            -- Here tail points (or pointed!) to the last node.  Try to link our new node.
            Null -> case casMutVar# nextMV next newp s3 of
                     (# s4, flag, newtail #) ->
                       if flag ==# 0#
                       then (# s4, tailTicket#, tail #)
                       else loop s4 newp 
            Cons _ _ -> 
               -- Someone has beat us by extending the tail.  Here we
               -- might have to do some community service by updating the tail ptr.
               case casMutVar# tailPtr tail next s3 of
                 (# s4, _, _ #) -> loop s4 newp 

-- tryPopR ::  LinkedQueue a -> IO (Maybe a)
-- tryPopR = error "tryPopR Unimplemented"

-- -- Andreas's checked this invariant in several places
-- -- Check for: head /= tail, and head->next == NULL
-- checkInvariant :: String -> LinkedQueue a -> IO ()
-- checkInvariant s (LQ headPtr tailPtr) = 
--   do head <- readIORef headPtr
--      tail <- readIORef tailPtr
--      if (not (pairEq head tail))
--        then case head of 
--               Null -> error (s ++ " checkInvariant: LinkedQueue invariants broken.  Internal error.")
--               Cons _ next -> do
--                 next' <- readIORef next
--                 case next' of 
--                   Null -> error (s ++ " checkInvariant: next' should not be null")
--                   _ -> return ()
--        else return ()


-- | Attempt to pop an element from the queue if one is available.
--   tryPop will return semi-promptly (depending on contention), but
--   will return 'Nothing' if the queue is empty.
tryPopR ::  LinkedQueue a -> IO (Maybe a)
-- FIXME / TODO -- add some kind of backoff.  This should probably at least
--   yield after a certain number of failures.
tryPopR q@(LQ headPtr tailPtr) = IO $ \st -> loop (0::Int) st
 where
   loop !tries st =
   {-           
#ifdef DEBUG
   --  loop 10 = do hPutStrLn stderr (pack "tryPopR: tried ~10 times!!");  loop 11 -- This one happens a lot on -N32
  loop 25   = do hPutStrLn stderr (pack "tryPopR: tried ~25 times!!");   loop 26
  loop 50   = do hPutStrLn stderr (pack "tryPopR: tried ~50 times!!");   loop 51
  loop 100  = do hPutStrLn stderr (pack "tryPopR: tried ~100 times!!");  loop 101
  loop 1000 = do hPutStrLn stderr (pack "tryPopR: tried ~1000 times!!"); loop 1001
#endif
   -}
      case readMutVar# headPtr st of
        (# st, head #) ->
          case readMutVar# tailPtr st of
            (# st, tail #) -> 
             case head of 
               Null -> error "tryPopR: LinkedQueue invariants broken.  Internal error."
               Cons _ next -> 
                case readMutVar# next st of
                  (# st, next' #) -> 
#ifdef RECHECK_ASSUMPTIONS
                   -- As with push, double-check our information is up-to-date. (head,tail,next consistent)
                   head' <- readIORef headPtr -- ANDREAS: used atomicModifyIORef headPtr (\x -> (x,x))
                   if not (pairEq head head') then loop (tries+1) else do 
#else
                   let head' = head in
#endif                 
                   -- Is queue empty or tail falling behind?:
                   if pairEq head tail then do 
                     case next' of -- Is queue empty?
                      Null -> (# st, Nothing #) -- Queue is empty, couldn't dequeue
                      Cons _ _ -> 
                        -- Tail is falling behind.  Try to advance it:
                        case casMutVar# tailPtr tail next' st of
                          (# st, _, _ #) -> loop (tries+1) st
                   else -- head /= tail
                    -- No need to deal with Tail.  Read value before CAS.
                    -- Otherwise, another dequeue might free the next node
                    case next' of 
                      Null -> error "tryPop: Internal error.  Next should not be null if head/=tail."
                      Cons value _ ->
                        -- Try to swing Head to the next node:
                        case casMutVar# headPtr head next' st of
                          (# st, b, _ #) -> 
                           if b ==# 0#
                           then (# st, Just value #) -- Dequeue done; exit loop.
                           else loop (tries+1) st


-- | Create a new queue.
newQ :: IO (LinkedQueue a)
newQ = IO$ \ s1 ->
  case newMutVar# Null s1 of
    (# s2, mv #) ->
     let newp = Cons (error "LinkedQueue: Used uninitialized magic value.") mv in
     case newMutVar# newp s2 of
       (# s3, hd #) ->
         case newMutVar# newp s3 of
          (# s4, tl #) -> (# s4, LQ hd tl #)

-- | Is the queue currently empty?  Beware that this can be a highly transient state.
nullQ :: LinkedQueue a -> IO Bool
nullQ (LQ headPtr tailPtr) = IO $ \ st ->
  case readMutVar# headPtr st of
    (# st, head #) ->
      case readMutVar# tailPtr st of
        (# st, tail #) -> (# st, pairEq head tail #)

--------------------------------------------------------------------------------
--   Instance(s) of abstract deque interface
--------------------------------------------------------------------------------

-- instance DequeClass (Deque T T S S Grow Safe) where 
instance C.DequeClass LinkedQueue where 
  newQ    = newQ
  nullQ   = nullQ
  pushL   = pushL
  tryPopR = tryPopR

--------------------------------------------------------------------------------

