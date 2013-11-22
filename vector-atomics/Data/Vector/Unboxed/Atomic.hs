{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Data.Vector.Unboxed.Atomic
       where

import Foreign.Storable (Storable, sizeOf)
import Data.Int
import Data.Word
import qualified Data.Vector.Primitive as P
-- The specific MV_* constructors that we need are here:
import Data.Vector.Unboxed.Base
import Data.Primitive (MutableByteArray)

import Data.Atomics (Ticket)

import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Bits.Atomic as BA
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import qualified Foreign.Ptr as P
  
--------------------------------------------------------------------------------

-- | Vector types which are implemented as sangle MutableByteArray, and whose
-- elements are of an appropriate size to perform atomic memory operations on them.
class IsOneMBV v a where
  getMutableByteArray :: v s a -> MutableByteArray s
  bitSize :: v s a -> Int

  default bitSize :: (IsOneMBV v a, Storable a) => v s a -> Int
  bitSize _ = sizeOf (undefined::a) * 8

{-
  bitSize :: a -> Int
  default bitSize :: (Storable a) => a -> Int
  bitSize _ = sizeOf (undefined::a) * 8

Huh, this ^^ attempt exposes some of the post-desugaring magic:

     Possible fix: add an instance declaration for (IsOneMBV v0 Int)
        In the expression: (Data.Vector.Unboxed.Atomic.$gdmbitSize)
        In an equation for `bitSize':
            bitSize = (Data.Vector.Unboxed.Atomic.$gdmbitSize)

-}


instance IsOneMBV MVector Int   where getMutableByteArray (MV_Int (P.MVector _ _ a)) = a
instance IsOneMBV MVector Int8  where getMutableByteArray (MV_Int8 (P.MVector _ _ a)) = a
instance IsOneMBV MVector Int16 where getMutableByteArray (MV_Int16 (P.MVector _ _ a)) = a
instance IsOneMBV MVector Int32 where getMutableByteArray (MV_Int32 (P.MVector _ _ a)) = a
instance IsOneMBV MVector Int64 where getMutableByteArray (MV_Int64 (P.MVector _ _ a)) = a

instance IsOneMBV MVector Word   where getMutableByteArray (MV_Word (P.MVector _ _ a)) = a
instance IsOneMBV MVector Word8  where getMutableByteArray (MV_Word8 (P.MVector _ _ a)) = a
instance IsOneMBV MVector Word16 where getMutableByteArray (MV_Word16 (P.MVector _ _ a)) = a
instance IsOneMBV MVector Word32 where getMutableByteArray (MV_Word32 (P.MVector _ _ a)) = a
instance IsOneMBV MVector Word64 where getMutableByteArray (MV_Word64 (P.MVector _ _ a)) = a

instance IsOneMBV MVector Bool   where getMutableByteArray (MV_Bool (P.MVector _ _ a)) = a
instance IsOneMBV MVector Char   where getMutableByteArray (MV_Char (P.MVector _ _ a)) = a
instance IsOneMBV MVector Double where getMutableByteArray (MV_Double (P.MVector _ _ a)) = a
instance IsOneMBV MVector Float  where getMutableByteArray (MV_Float (P.MVector _ _ a)) = a

--------------------------------------------------------------------------------

-- | A class for vectors whose contents are unboxed numbers, not Haskell heap objects.
class AtomicUVec v a where
  fetchAndAdd  :: v s a -> Int -> a -> IO a
  fetchAndSub  :: v s a -> Int -> a -> IO a
  fetchAndOr   :: v s a -> Int -> a -> IO a
  fetchAndAnd  :: v s a -> Int -> a -> IO a
  fetchAndXor  :: v s a -> Int -> a -> IO a
  fetchAndNand :: v s a -> Int -> a -> IO a
  addAndFetch  :: v s a -> Int -> a -> IO a
  subAndFetch  :: v s a -> Int -> a -> IO a
  orAndFetch   :: v s a -> Int -> a -> IO a
  andAndFetch  :: v s a -> Int -> a -> IO a
  xorAndFetch  :: v s a -> Int -> a -> IO a
  nandAndFetch :: v s a -> Int -> a -> IO a
  -- lockTestAndSet :: v s a -> Int -> IO a
  -- lockRelease :: v s a -> Int -> IO ()

  compareAndSwap :: v s a -> Int -> a -> a -> IO (Maybe a)

-- | Atomic operations on /boxed/ vectors containing arbitrary Haskell values.
class AtomicVec v a where
  compareAndSwapT :: v s a -> Int -> Ticket a -> a -> IO (Bool, Ticket a)

-- 
-- 

boundsCheck :: (Num a, Ord a) => String -> a -> a -> t -> t
boundsCheck name ix len x
  | ix >= 0 && ix < len = x
  | otherwise = error $ name ++": index out of bounds "

-- FIXME: BOUNDS CHECK:

#define DOOP(name) \
  name (SM.MVector len fp) ix val = \
    withForeignPtr fp (\ptr ->       \
      let offset = sizeOf (undefined::elt) * ix in \
      boundsCheck "atomic vector op" ix len  \
      (BA.name (P.plusPtr ptr offset) val ))

instance (Storable elt, BA.AtomicBits elt) =>
         AtomicUVec SM.MVector elt where
  DOOP(fetchAndAdd)
  DOOP(fetchAndSub)
  DOOP(fetchAndOr)
  DOOP(fetchAndAnd)
  DOOP(fetchAndXor)
  DOOP(fetchAndNand)
  DOOP(addAndFetch)
  DOOP(subAndFetch)
  DOOP(orAndFetch)
  DOOP(andAndFetch)
  DOOP(xorAndFetch) 
  DOOP(nandAndFetch)  

  compareAndSwap (SM.MVector _len fp) ix old new = 
    withForeignPtr fp $ \ptr -> do
      let offset = sizeOf (undefined::elt) * ix
      old' <- BA.compareAndSwap (P.plusPtr ptr offset) old new
      return $! if old' == old
                then Nothing
                else Just old'

  -- compareAndSwapT arr ix tick new =
  --   error "FINISHME - compareAndSwapT "
  
-- instance AtomicUVec SM.MVector Int where
