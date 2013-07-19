{-# LANGUAGE FlexibleInstances, NamedFieldPuns, CPP, ScopedTypeVariables, BangPatterns, MagicHash #-}

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
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
-- import Data.Vector.Unboxed.Mutable as V
-- import Data.Vector
import Text.Printf (printf)
import Control.Exception (catch, SomeException, throw, evaluate,try)
import Control.Monad (when, unless, forM_)

import Data.Atomics (storeLoadBarrier, writeBarrier, loadLoadBarrier)
-- TODO: Use whichever counter is exported as the DEFAULT:
import Data.Atomics.Counter.Unboxed
       (AtomicCounter, newCounter, readCounter, writeCounter, casCounter, readCounterForCAS, peekCTicket)

-- Debugging:
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import System.Mem.StableName (makeStableName, hashStableName)
import GHC.Exts (Int(I#))
import GHC.Prim (reallyUnsafePtrEquality#, unsafeCoerce#)

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
  , activeArr :: {-# UNPACK #-} !(IORef (MV.IOVector a))
  }

dbgInspectCLD :: Show a => ChaseLevDeque a -> IO String
dbgInspectCLD CLD{top,bottom,activeArr} = do
  tp <- readCounter top
  bt <- readCounter bottom
  vc <- readIORef activeArr
  elems  <- fmap V.toList$ V.freeze vc
  elems' <- mapM safePrint elems
  let sz = MV.length vc
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

{-# INLINE rd #-}
{-# INLINE wr #-}
{-# INLINE nu #-}
{-# INLINE cpy #-}
{-# INLINE slc #-}
#ifndef DEBUGCL
dbg = False
nu  = MV.unsafeNew
rd  = MV.unsafeRead
wr  = MV.unsafeWrite
slc = MV.unsafeSlice
cpy = MV.unsafeCopy
#else
#warning "Activating DEBUGCL!"
dbg = True
nu  = MV.new 
rd  = MV.read
slc = MV.slice
cpy = MV.copy
wr  = MV.write
-- Temp, debugging: Our own bounds checking, better error:
-- wr v i x = 
--   if i >= MV.length v
--   then error (printf "ERROR: Out of bounds of top of vector index %d, vec length %d\n" i (MV.length v))
--   else MV.write v i x

-- [2013.06.25] Note Issue5 is not affected by this:
{-# NOINLINE pushL #-}
{-# NOINLINE tryPopL #-}
{-# NOINLINE tryPopR #-}
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
growCirc :: Int -> Int -> MV.IOVector a -> IO (MV.IOVector a)
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
  let len   = MV.length oldarr
      elems = end - strt
  -- putStrLn$ "Grow to size "++show (len+len)++", copying over "++show elems
  newarr <- if dbg then
               nu (len + len)
            else  -- Better errors:
                V.thaw $ V.generate (len+len) (\i -> error (" uninitialized element at position " ++ show i
							    ++" had only initialized "++show elems++" elems: "
							    ++show(strt`mod`(len+len),end`mod`(len+len))))
  -- Strictly matches what's in the paper:
  for_ strt end $ \ind -> do 
    x <- getCirc oldarr ind 
    evaluate x
    putCirc newarr ind x
  return $! newarr
{-# INLINE growCirc #-}

getCirc :: MV.IOVector a -> Int -> IO a
getCirc !arr !ind   = rd arr (ind `mod` MV.length arr)
{-# INLINE getCirc #-}

putCirc :: MV.IOVector a -> Int -> a -> IO ()
putCirc !arr !ind x = wr arr (ind `mod` MV.length arr) x
{-# INLINE putCirc #-}

-- Use a potentially-optimized block-copy:
copyOffset :: MV.IOVector t -> MV.IOVector t -> Int -> Int -> Int -> IO ()
copyOffset !from !to !iFrom !iTo !len =
  cpy (slc iTo len to)
      (slc iFrom len from)
{-# INLINE copyOffset #-}


--------------------------------------------------------------------------------
-- Queue Operations
--------------------------------------------------------------------------------

newQ :: IO (ChaseLevDeque elt)
newQ = do
  -- Arbitrary Knob: We start as size 32 and double from there:
  v  <- MV.new 32 
  r1 <- newCounter 0
  r2 <- newCounter 0
  r3 <- newIORef v
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
  arr <- readIORef activeArr
  let len = MV.length arr 
      size = b - t

--  when (dbg && size < 0) $ error$ "pushL: INVARIANT BREAKAGE - bottom, top: "++ show (b,t)

  arr' <- if (size >= len - 1) then do 
            arr' <- growCirc t b arr -- Double in size, don't change b/t.
            -- Only a single thread will do this!:
	    writeIORef activeArr arr'
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
  arr <- readIORef activeArr
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
  arr <- readIORef activeArr
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
      return $! Just obj
     else do
      (b,ol) <- casCounter top tt (t+1)
      writeCounter bottom (t+1)
      if b then return $! Just obj
           else return $  Nothing 

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
