{-# LANGUAGE  MagicHash, UnboxedTuples, BangPatterns #-}

module Data.Atomics 
 (
   -- * Types for atomic operations
   Ticket, CASResult(..),

   -- * Atomic operations on mutable arrays
   casArrayElem,

   -- * Atomic operations on IORefs
   readForCAS, casIORef,

   -- * Atomic operations on STRefs
--   readSTRefForCAS, casSTRef,
   
   -- * Atomic operations on raw MutVars
   readMutVarForCAS, casMutVar
      
 ) where

import Control.Monad.ST (stToIO)
import Data.Primitive.Array (MutableArray(MutableArray))
import Data.Atomics.Internal (casArray#, readForCAS#, casMutVar2#, Ticket)
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

--------------------------------------------------------------------------------

-- | A compare and swap operation may fail.  If so, it returns an
--   observation of the current value.  Also, it always returns a
--   ticket for future operations on the same location.
data CASResult a = Fail    {-# UNPACK #-} !Ticket !a
                 | Succeed {-# UNPACK #-} !Ticket
  deriving (Show, Eq, Read, Ord)


--------------------------------------------------------------------------------

{-# INLINE casArrayElem #-}
casArrayElem :: MutableArray RealWorld a -> Int -> a -> a -> IO (Bool, a)
casArrayElem arr i old new = stToIO (casArrayST arr i old new)

{-# INLINE casArrayST #-}
-- -- | Write a value to the array at the given index:
-- casArrayST :: MutableArray s a -> Int -> a -> a -> ST s (Bool, a)
casArrayST :: MutableArray RealWorld a -> Int -> a -> a -> ST RealWorld (Bool, a)
casArrayST (MutableArray arr#) (I# i#) old new = ST$ \s1# ->
 case casArray# arr# i# old new s1# of 
   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)


--------------------------------------------------------------------------------

readForCAS :: IORef a -> IO ( Ticket, a )
readForCAS (IORef (STRef mv)) = readMutVarForCAS mv

-- | Performs a machine-level compare and swap operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'IORef'.
-- 
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.
casIORef :: IORef a -- ^ The 'IORef' containing a value 'current'
         -> Ticket  -- ^ A ticket for the 'old' value
         -> a       -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (CASResult a)
casIORef (IORef (STRef var)) old new = casMutVar var old new

--------------------------------------------------------------------------------

-- readSTRefForCAS = undefined

-- | Performs a machine-level compare and swap operation on an
-- 'STRef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'STRef'.
-- 
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.


-- casSTRef :: STRef s a -- ^ The 'STRef' containing a value 'current'
--          -> Ticket -- ^ A ticket for the 'old' value
--          -> a      -- ^ The 'new' value to replace 'current' if @old == current@
--          -> ST s (CASResult a)
-- casSTRef (STRef var#) old new = casMutVar var# old new

--  ST $ \s1# ->  
   -- -- The primop treats the boolean as a sort of error code.
   -- -- Zero means the CAS worked, one that it didn't.
   -- -- We flip that here:
--    case casMutVar# var# old new s1# of
--      (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)
      
--------------------------------------------------------------------------------

{-# INLINE readForCAS #-}
readMutVarForCAS :: MutVar# RealWorld a -> IO ( Ticket, a )
readMutVarForCAS mv = IO$ \ st -> 
  case readForCAS# mv st of 
   (# st, tick, val #) -> (# st, (W# tick, val) #)


{-# INLINE casMutVar #-}
casMutVar :: MutVar# RealWorld a -> Ticket -> a -> IO (CASResult a)
casMutVar mv (W# tick#) new = IO$ \st -> 
  case casMutVar2# mv tick# new st of 
    (# st, flag, tick', val #) -> 
      if flag ==# 0# 
      then (# st, Succeed (W# tick') #)
      else (# st, Fail (W# tick') val #)


