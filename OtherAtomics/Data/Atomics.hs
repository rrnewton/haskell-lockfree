{-# LANGUAGE  MagicHash, UnboxedTuples #-}

module Data.Atomics 
 (casArrayElem, 
  readForCAS, Ticket
 ) where

import Control.Monad.ST (stToIO)
import Data.Primitive.Array (MutableArray(MutableArray))
import Data.Atomics.Internal (casArray#, readForCAS#, Ticket)
import Data.Int -- TEMPORARY

import GHC.ST
import GHC.Prim
import GHC.Arr 
import GHC.Base (Int(I#))
import GHC.IO (IO(IO))
import GHC.Word (Word(W#))

{-# INLINE casArrayST #-}

-- -- | Write a value to the array at the given index:
-- casArrayST :: MutableArray s a -> Int -> a -> a -> ST s (Bool, a)
casArrayST :: MutableArray RealWorld a -> Int -> a -> a -> ST RealWorld (Bool, a)
casArrayST (MutableArray arr#) (I# i#) old new = ST$ \s1# ->
 case casArray# arr# i# old new s1# of 
   (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

{-# INLINE casArrayElem #-}
casArrayElem :: MutableArray RealWorld a -> Int -> a -> a -> IO (Bool, a)
casArrayElem arr i old new = stToIO (casArrayST arr i old new)


{-# INLINE readForCAS #-}
readForCAS :: MutVar# RealWorld a -> IO ( Ticket, a )
readForCAS mv = IO$ \ st -> 
  case readForCAS# mv st of 
   (# st, tick, val #) -> (# st, (W# tick, val) #)

