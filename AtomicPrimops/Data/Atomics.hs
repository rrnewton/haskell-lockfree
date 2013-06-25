{-# LANGUAGE  MagicHash, UnboxedTuples, BangPatterns, ScopedTypeVariables, CPP #-}

-- | Provides atomic memory operations on IORefs and Mutable Arrays.
--
--   Pointer equality need not be maintained by a Haskell compiler.  For example, Int
--   values will frequently be boxed and unboxed, changing the pointer identity of
--   the thunk.  To deal with this, the compare-and-swap (CAS) approach used in this
--   module is uses a /sealed/ representation of pointers into the Haskell heap
--   (`Tickets`).  Currently, the user cannot coin new tickets, rather a `Ticket`
--   provides evidence of a past observation, and grants permission to make a future
--   change.
module Data.Atomics 
 (
   -- * Types for atomic operations
   Ticket, peekTicket, -- CASResult(..),

   -- * Atomic operations on mutable arrays
   casArrayElem, casArrayElem2, readArrayElem, 

   -- * Atomic operations on IORefs
   readForCAS, casIORef, casIORef2, 
   
   -- * Atomic operations on raw MutVars
   readMutVarForCAS, casMutVar, casMutVar2
      
 ) where

import Control.Monad.ST (stToIO)
import Data.Primitive.Array (MutableArray(MutableArray))
import Data.Atomics.Internal (casArray#, readForCAS#, casMutVarTicketed#, Ticket)
import Data.Int -- TEMPORARY

import Data.IORef
import GHC.IORef
import GHC.STRef
import GHC.ST
import GHC.Prim
import GHC.Arr 
import GHC.Base (Int(I#))
import GHC.IO (IO(IO))
import GHC.Word (Word(W#))

#ifdef DEBUG_ATOMICS
#warning "Activating DEBUG_ATOMICS..."
-- [2013.06.25] Changing this to NOINLINE for debugging...
{-# NOINLINE casIORef #-} -- Note, it doesn't fix issue5.

{-# NOINLINE seal #-}
#else

#endif

--------------------------------------------------------------------------------

{-# INLINE casArrayElem #-}
-- | Compare-and-swap 
casArrayElem :: MutableArray RealWorld a -> Int -> Ticket a -> a -> IO (Bool, Ticket a)
-- casArrayElem (MutableArray arr#) (I# i#) old new = IO$ \s1# ->
--  case casArray# arr# i# old new s1# of 
--    (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)
casArrayElem arr i old new = casArrayElem2 arr i old (seal new)

{-# INLINE casArrayElem2 #-}   
-- | This variant takes two tickets: the 'new' value is a ticket rather than an
-- arbitrary, lifted, Haskell value.
casArrayElem2 :: MutableArray RealWorld a -> Int -> Ticket a -> Ticket a -> IO (Bool, Ticket a)
casArrayElem2 (MutableArray arr#) (I# i#) old new = IO$ \s1# ->
 case casArray# arr# i# old new s1# of 
   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)


{-# INLINE readArrayElem #-}
readArrayElem :: forall a . MutableArray RealWorld a -> Int -> IO (Ticket a)
-- readArrayElem = unsafeCoerce# readArray#
readArrayElem (MutableArray arr#) (I# i#) = IO $ \ st -> unsafeCoerce# (fn st)
  where
    fn :: State# RealWorld -> (# State# RealWorld, a #)
    fn = readArray# arr# i#


--------------------------------------------------------------------------------

{-# INLINE readForCAS #-}
readForCAS :: IORef a -> IO ( Ticket a )
readForCAS (IORef (STRef mv)) = readMutVarForCAS mv

{-# INLINE casIORef #-}
-- | Performs a machine-level compare and swap operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'IORef'.
-- 
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.
casIORef :: IORef a  -- ^ The 'IORef' containing a value 'current'
         -> Ticket a -- ^ A ticket for the 'old' value
         -> a        -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (Bool, Ticket a)
casIORef (IORef (STRef var)) old new = casMutVar var old new 


{-# INLINE casIORef2 #-}
-- | This variant takes two tickets, i.e. the 'new' value is a ticket rather than an
-- arbitrary, lifted, Haskell value.
casIORef2 :: IORef a 
         -> Ticket a -- ^ A ticket for the 'old' value
         -> Ticket a -- ^ A ticket for the 'new' value
         -> IO (Bool, Ticket a)
casIORef2 (IORef (STRef var)) old new = casMutVar2 var old new 


--------------------------------------------------------------------------------

-- | A ticket contains or can get the usable Haskell value.
{-# NOINLINE peekTicket #-}
peekTicket :: Ticket a -> a 
peekTicket = unsafeCoerce#

-- Not exposing this for now.  Presently the idea is that you must read from the
-- mutable data structure itself to get a ticket.
seal :: a -> Ticket a 
seal = unsafeCoerce#


{-# INLINE readMutVarForCAS #-}
readMutVarForCAS :: MutVar# RealWorld a -> IO ( Ticket a )
readMutVarForCAS !mv = IO$ \ st -> readForCAS# mv st

{-# INLINE casMutVar #-}
-- | MutVar counterpart of `casIORef`.
--
casMutVar :: MutVar# RealWorld a -> Ticket a -> a -> IO (Bool, Ticket a)
casMutVar !mv !tick !new = casMutVar2 mv tick (seal new)

{-# INLINE casMutVar2 #-}
-- | This variant takes two tickets, i.e. the 'new' value is a ticket rather than an
-- arbitrary, lifted, Haskell value.
casMutVar2 :: MutVar# RealWorld a -> Ticket a -> Ticket a -> IO (Bool, Ticket a)
casMutVar2 !mv !tick !new = IO$ \st -> 
  case casMutVarTicketed# mv tick new st of 
    (# st, flag, tick' #) ->
      (# st, (flag ==# 0#, tick') #)
--      (# st, if flag ==# 0# then Succeed tick' else Fail tick' #)
--      if flag ==# 0#    then       else (# st, Fail (W# tick')  #)


