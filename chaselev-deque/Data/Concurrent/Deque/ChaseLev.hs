{-# LANGUAGE FlexibleInstances, NamedFieldPuns, CPP, ScopedTypeVariables, BangPatterns, MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Chase-Lev work stealing Deques
--
-- This implementation derives directly from the pseudocode in the 2005 SPAA paper:
--
--   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.1097&rep=rep1&type=pdf
--
-- TODO: local topBound optimization.
-- TODO: Do the more optimized version of growCirc
module Data.Concurrent.Deque.ChaseLev
  (
    -- The convention here is to directly provide the concrete
    -- operations as well as providing the class instances.
    ChaseLevDeque(), newQ, nullQ, pushL, tryPopL, tryPopR,
    approxSize,
    dbgInspectCLD
  )
 where

import Data.IORef
import Data.List (isInfixOf, intersperse)
import qualified Data.Concurrent.Deque.Class as PC

-- import Data.CAS (casIORef)
--import qualified Data.Vector.Mutable as MV
--import qualified Data.Vector as V
-- import Data.Vector.Unboxed.Mutable as V
-- import Data.Vector
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.UnliftedArray (PrimUnlifted (..))
import Data.Primitive.MutVar
import Text.Printf (printf)
import Control.Exception (catch, SomeException, throw, evaluate,try)
import Control.Monad (when, unless, forM_)

import Data.Atomics (storeLoadBarrier, writeBarrier, loadLoadBarrier)
import Data.Atomics.Counter
       (AtomicCounter, newCounter, readCounter, writeCounter, casCounter, readCounterForCAS, peekCTicket)

-- Debugging:
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import System.Mem.StableName (makeStableName, hashStableName)
import GHC.Exts (Int(I#), RealWorld, Any)
import GHC.Prim (reallyUnsafePtrEquality#, unsafeCoerce#)
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Instances

instance PC.DequeClass ChaseLevDeque where
  newQ  = newQ
  nullQ = nullQ
  pushL = pushL
  tryPopR = tryPopR
  -- | Popping the left end is the "local" side:
  leftThreadSafe  _ = False
  rightThreadSafe _ = True

instance PC.PopL ChaseLevDeque where
  tryPopL = tryPopL

--------------------------------------------------------------------------------
-- Type definition

data ChaseLevDeque a = CLD {
    top       :: {-# UNPACK #-} !AtomicCounter
  , bottom    :: {-# UNPACK #-} !AtomicCounter
    -- This is a circular array:
  , activeArr :: {-# UNPACK #-} !(ArrayRef RealWorld a)
  }

-- We pull a dirty trick to store a MutableArray# in a MutVar#.
-- ArrayRef s a is a reference to an array of elements of type a.
newtype ArrayRef s a = ArrayRef (MutVar s Any)
type role ArrayRef nominal representational

readArrayRef :: PrimMonad m
                => ArrayRef (PrimState m) a -> m (MutableArray (PrimState m) a)
readArrayRef (ArrayRef ref) = do
  a <- readMutVar ref
  return (MutableArray (unsafeCoerce# a))

writeArrayRef :: PrimMonad m
                 => ArrayRef (PrimState m) a -> MutableArray (PrimState m) a -> m ()
writeArrayRef (ArrayRef ref) (MutableArray a) =
  writeMutVar ref (unsafeCoerce# a)

{-
sameArrayRef :: ArrayRef s a -> ArrayRef s a -> Bool
sameArrayRef (ArrayRef ref1) (ArrayRef ref2) =
  sameMutableUnliftedArray ref1 ref2
-}

newArrayRef :: PrimMonad m
            => MutableArray (PrimState m) a -> m (ArrayRef (PrimState m) a)
newArrayRef (MutableArray a) = ArrayRef <$> newMutVar (unsafeCoerce# a)

dbgInspectCLD :: Show a => ChaseLevDeque a -> IO String
dbgInspectCLD CLD{top,bottom,activeArr} = do
  tp <- readCounter top
  bt <- readCounter bottom
  vc <- readArrayRef activeArr
  elems  <- fmap F.toList $ freezeArray vc 0 (sizeofMutableArray vc)
  elems' <- mapM safePrint elems
  let sz = sizeofMutableArray vc
  return$ "  {DbgInspectCLD: top "++show tp++", bot "++show bt++", size "++show sz++"\n" ++
          -- show elems ++ "\n"++
          "   [ "++(concat $ intersperse " " elems')++" ]\n"++
          "  end_DbgInspectCLD}"
 where
   -- Print any thunk, even if it raises an exception.
   safePrint :: Show a => a -> IO String
   safePrint val = do
     res <- try (evaluate val)
     case res of
       Left (e::SomeException)
         | isInfixOf "uninitialised element" (show e) -> return "<uninit>"
         | otherwise -> return$ "<"++ show e ++">"
       Right val' -> return (show val')



--------------------------------------------------------------------------------
-- Debugging mode.
-- define DEBUGCL
--define FAKECAS

#ifndef DEBUGCL
dbg = False
nu  = newArray
rd  = readArray
wr  = writeArray
#else
#warning "Activating DEBUGCL!"
dbg = True
nu i a
  | i >= 0 = newArray i a
  | otherwise = error "chaselev-deque: negative argument to nu"
rd mary i
  | 0 <= i && i <= sizeofMutableArray mary
  = readArray mary i
  | otherwise = error "chaselev-deque: rd out of range."
wr mary i x
  | 0 <= i && i <= sizeofMutableArray mary
  = writeArray mary i x
  | otherwise = error "chaselev-deque: wr out of range."
-- Temp, debugging: Our own bounds checking, better error:
-- wr v i x =
--   if i >= sizeofMutableArray v
--   then error (printf "ERROR: Out of bounds of top of vector index %d, vec length %d\n" i (sizeofMutableArray v))
--   else writeMutableArray v i x
#endif


#ifdef DEBUGCL
-- This simply localizes exceptions better:
tryit msg action = Control.Exception.catch action
	                        (\e -> do putStrLn$ "ERROR inside "++msg++" "++ show e
                                          throw (e::SomeException))
#else
{-# INLINE tryit #-}
tryit msg action = action
#endif



--------------------------------------------------------------------------------
-- Circular array routines:


-- TODO: make a "grow" that uses memcpy.
growCirc :: Int -> Int -> MutableArray RealWorld a -> IO (MutableArray RealWorld a)
growCirc !strt !end !oldarr = do
  -- let len = MV.length oldarr
  --     strtmod = strt`mod` len
  --     endmod  = end `mod` len
  -- newarr <- nu (len + len)
  -- if endmod < strtmod then do
  --   let elems1 = len - strtmod
  --       elems2 = endmod
  --   BS.putStrLn$ BS.pack$ printf "Copying segmented ... %d and %d" elems1 elems2

  --   -- Copy the upper then lower segments:
  --   copyOffset oldarr newarr   strtmod  0       elems1
  --   copyOffset oldarr newarr   0        elems1  elems2
  --  else do
  --   BS.putStrLn$ BS.pack$ printf "Copying one seg into vec of size %d... size %d, strt %d, end %d, strtmod %d endmod %d" (MV.length newarr) (end - strt) strt end strtmod endmod
  --   -- Copy a single segment:
  --   copyOffset oldarr newarr strtmod 0 (end - strt)
  -- return newarr
  ----------------------------------------
  -- Easier version first:
  ----------------------------------------
  let len   = sizeofMutableArray oldarr
      elems = end - strt
  when dbg $ putStrLn$ "Grow to size "++show (len+len)++", copying over "++show elems
  newarr <- nu (len + len) $ error "uninitialized element following grow"
  when dbg $
      -- better errors
    for_ 0 (len + len) $ \ind ->
      writeArray newarr ind $ error $
        "uninitialized element at position "
          ++ show ind
          ++ " had only initialized " ++ show elems ++ " elems: "
          ++ show (strt `mod` (len + len), end `mod` (len + len))

  if strt <= end
    then
      do
        -- TODO: We should consider using copyMutableArray here, but
        -- that involves some fancy indexing calculations.
        for_ strt end $ \ind -> do
          x <- getCirc oldarr ind
          evaluate x
          putCirc newarr ind x
        return newarr
    else error "ChaseLev invariant failure in growCirc"
{-# INLINE growCirc #-}

getCirc :: MutableArray RealWorld a -> Int -> IO a
getCirc !arr !ind   = rd arr (ind `mod` sizeofMutableArray arr)
{-# INLINE getCirc #-}

putCirc :: MutableArray RealWorld a -> Int -> a -> IO ()
putCirc !arr !ind x = wr arr (ind `mod` sizeofMutableArray arr) x
{-# INLINE putCirc #-}

{-
-- Use a potentially-optimized block-copy:
copyOffset :: MutableArray RealWorld t -> MutableArray RealWorld t -> Int -> Int -> Int -> IO ()
copyOffset !from !to !iFrom !iTo !len =
  cpy (slc iTo len to)
      (slc iFrom len from)
{-# INLINE copyOffset #-}
-}


--------------------------------------------------------------------------------
-- Queue Operations
--------------------------------------------------------------------------------

newQ :: IO (ChaseLevDeque elt)
newQ = do
  -- Arbitrary Knob: We start as size 32 and double from there:
  v  <- newArray 32 $ error "newQ: uninitialized element from beginning"
  r1 <- newCounter 0
  r2 <- newCounter 0
  r3 <- newArrayRef v
  return $! CLD r1 r2 r3

{-# INLINE newQ #-}
nullQ :: ChaseLevDeque elt -> IO Bool
nullQ CLD{top,bottom} = do
  -- This should get a LOWER bound on size at some point in logic time, right?
  b   <- readCounter bottom
  t   <- readCounter top
  let size = b - t
  return $! size <= 0

{-# INLINE approxSize #-}
-- | Return a lower bound on the size at some point in the recent past.
approxSize :: ChaseLevDeque elt -> IO Int
approxSize CLD{top,bottom} = do
  b   <- readCounter bottom
  t   <- readCounter top
  return $! b - t

{-# INLINE pushL #-}
-- | For a work-stealing queue `pushL` is the ``local'' push.  Thus
--   only a single thread should perform this operation.
pushL :: ChaseLevDeque a -> a  -> IO ()
pushL CLD{top,bottom,activeArr} obj = tryit "pushL" $ do
  b   <- readCounter bottom
  t   <- readCounter top
  arr <- readArrayRef activeArr
  let len = sizeofMutableArray arr
      size = b - t

--  when (dbg && size < 0) $ error$ "pushL: INVARIANT BREAKAGE - bottom, top: "++ show (b,t)

  arr' <- if (size >= len - 1) then do
            arr' <- growCirc t b arr -- Double in size, don't change b/t.
            -- Only a single thread will do this!:
            writeArrayRef activeArr arr'
            return arr'
          else return arr

  putCirc arr' b obj
  {-
     KG: we need to put write barrier here since otherwise we might
     end with elem not added to q->elements, but q->bottom already
     modified (write reordering) and with stealWSDeque_ failing
     later when invoked from another thread since it thinks elem is
     there (in case there is just added element in the queue). This
     issue concretely hit me on ARMv7 multi-core CPUs
   -}
  writeBarrier
  writeCounter bottom (b+1)
  return ()

-- {-# INLINE tryPopR #-}
-- | This is the steal operation.  Multiple threads may concurrently
-- attempt steals from the same thread.
tryPopR :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopR CLD{top,bottom,activeArr} =  tryit "tryPopR" $ do
  -- NB. these loads must be ordered, otherwise there is a race
  -- between steal and pop.
  tt  <- readCounterForCAS top
  loadLoadBarrier
  b   <- readCounter bottom
  arr <- readArrayRef activeArr
 -- when (dbg && b < t) $ error$ "tryPopR: INVARIANT BREAKAGE - bottom < top: "++ show (b,t)

  let t = peekCTicket tt
      size = b - t
  if size <= 0 then
    return Nothing
   else do
    obj   <- getCirc  arr t
    (b,_) <- casCounter top tt (t+1)
    if b then
      return $! Just obj
     else
      return Nothing -- Someone beat us, abort

{-# INLINE tryPopL #-}
tryPopL  :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopL CLD{top,bottom,activeArr} = tryit "tryPopL" $ do
  b   <- readCounter bottom
  arr <- readArrayRef activeArr
  b   <- evaluate (b-1)
  writeCounter bottom b

  -- very important that the following read of q->top does not occur
  -- before the earlier write to q->bottom.
  storeLoadBarrier

  tt   <- readCounterForCAS top
--  when (dbg && b < t) $ error$ "tryPopL: INVARIANT BREAKAGE - bottom < top: "++ show (b,t)

  let t = peekCTicket tt
      size = b - t
  if size < 0 then do
    writeCounter bottom t
    return Nothing
   else do
    obj <- getCirc arr b
    if size > 0 then do
      return $ Just obj
     else do
      (b,ol) <- casCounter top tt (t+1)
      writeCounter bottom (t+1)
      if b then return $ Just obj
           else return $ Nothing

------------------------------------------------------------

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ !start !end _fn | start > end = error "for_: start is greater than end"
for_ !start !end fn = loop start
  where
   loop !i | i == end  = return ()
           | otherwise = do fn i; loop (i+1)
