{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Vector.Unboxed.Atomic
       where

import Data.Int
import Data.Word
import qualified Data.Vector.Primitive as P
-- The specific MV_* constructors that we need are here:
import Data.Vector.Unboxed.Base
import Data.Primitive (MutableByteArray)

-- | Vector types which are implemented as MutableByteArray, and whose elements are
-- of an appropriate size to perform atomic memory operations on them.
class AtomicVec v a where
  getMutableByteArray :: v s a -> MutableByteArray s

instance AtomicVec MVector Int   where getMutableByteArray (MV_Int (P.MVector _ _ a)) = a
instance AtomicVec MVector Int8  where getMutableByteArray (MV_Int8 (P.MVector _ _ a)) = a
instance AtomicVec MVector Int16 where getMutableByteArray (MV_Int16 (P.MVector _ _ a)) = a
instance AtomicVec MVector Int32 where getMutableByteArray (MV_Int32 (P.MVector _ _ a)) = a
instance AtomicVec MVector Int64 where getMutableByteArray (MV_Int64 (P.MVector _ _ a)) = a

instance AtomicVec MVector Word   where getMutableByteArray (MV_Word (P.MVector _ _ a)) = a
instance AtomicVec MVector Word8  where getMutableByteArray (MV_Word8 (P.MVector _ _ a)) = a
instance AtomicVec MVector Word16 where getMutableByteArray (MV_Word16 (P.MVector _ _ a)) = a
instance AtomicVec MVector Word32 where getMutableByteArray (MV_Word32 (P.MVector _ _ a)) = a
instance AtomicVec MVector Word64 where getMutableByteArray (MV_Word64 (P.MVector _ _ a)) = a
