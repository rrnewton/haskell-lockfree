{-# LANGUAGE  MagicHash, UnboxedTuples #-}

module Data.Atomics (casArrayElem) where

import Data.Atomics.Internal (casArray#)
import GHC.ST
import GHC.Prim
import GHC.Arr 
import GHC.Base (Int(I#))
import Data.Primitive.Array (MutableArray(MutableArray))
import Control.Monad.ST (stToIO)

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

-- stToIO (casSTRef var old new)
