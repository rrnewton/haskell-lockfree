{-# LANGUAGE FlexibleInstances, NamedFieldPuns, CPP #-}

-- | Chase-Lev work stealing Deques
-- 
-- This implementation derives directly from the pseudocode in the 2005 SPAA paper:
--
--   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.1097&rep=rep1&type=pdf
--
module Data.Concurrent.Deque.ChaseLev 
  (
    -- The convention here is to directly provide the concrete
    -- operations as well as providing the class instances.
    ChaseLevDeque(), newQ, nullQ, pushL, tryPopL, tryPopR
  )
 where

import Data.IORef
import qualified Data.Concurrent.Deque.Class as PC

import Data.CAS (casIORef)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
-- import Data.Vector.Unboxed.Mutable as V
-- import Data.Vector
import Text.Printf (printf)
import Control.Exception(catch, SomeException, throw)
import Control.Monad (when, unless, forM_)
-- import Control.Monad.ST

import qualified Data.ByteString.Char8 as BS

--------------------------------------------------------------------------------
-- Instances

instance PC.DequeClass ChaseLevDeque where 
  newQ  = newQ
  nullQ = nullQ
  pushL = pushL
  tryPopR = tryPopR

instance PC.PopL ChaseLevDeque where 
  tryPopL = tryPopL

--------------------------------------------------------------------------------
-- Type definition

data ChaseLevDeque a = CLD {
    top       :: {-# UNPACK #-} !(IORef Int)
  , bottom    :: {-# UNPACK #-} !(IORef Int)
    -- This is a circular array:
  , activeArr :: {-# UNPACK #-} !(IORef (MV.IOVector a))
  }

--------------------------------------------------------------------------------
-- Debugging mode.
#define DEBUG

{-# INLINE rd #-}
{-# INLINE wr #-}
#ifndef DEBUG
nu  = MV.unsafeNew
rd  = MV.unsafeRead
wr  = MV.unsafeWrite
#else
nu  = MV.new 
rd  = MV.read
-- wr  = MV.write

-- Our own bounds checking:
wr v i x = 
  -- [2012.03.08] We are indeed seeing a off-by-one error:
  if i >= MV.length v
  then error (printf "ERROR: Out of bounds of top of vector index %d, vec length %d\n" i (MV.length v))
  else MV.write v i x 
#endif



#ifdef DEBUG
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
growCirc strt end oldarr = do  
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


  -- Easier version first:
  let len   = MV.length oldarr
      elems = end - strt
  newarr <- nu (len + len)
  forM_ [1..elems] $ \ind -> do 
    x <- getCirc oldarr ind 
    putCirc newarr ind x
  return newarr


{-# INLINE growCirc #-}

getCirc arr ind   = rd arr (ind `mod` MV.length arr)
putCirc arr ind x = wr arr (ind `mod` MV.length arr) x

{-# INLINE getCirc #-}
{-# INLINE putCirc #-}


-- t1 = 


-- copyOffset :: MV.MVector s t -> MV.MVector s t -> Int -> Int -> Int -> ST s ()
copyOffset :: MV.IOVector t -> MV.IOVector t -> Int -> Int -> Int -> IO ()
#ifdef DEBUG
copyOffset from to iFrom iTo len =
  MV.copy (MV.slice iTo len to)
	  (MV.slice iFrom len from)
#else 
copyOffset from to iFrom iTo len =
  MV.unsafeCopy (MV.unsafeSlice iTo len to)
	        (MV.unsafeSlice iFrom len from)
#endif
{-# INLINE copyOffset #-}


--------------------------------------------------------------------------------
-- Queue Operations

-- logInitialSize
newQ :: IO (ChaseLevDeque elt)
newQ = do
  -- We start as size 32 and double from there:
  v  <- MV.new 32 
  r1 <- newIORef 0
  r2 <- newIORef 0
  r3 <- newIORef v
  return$ CLD r1 r2 r3

nullQ :: ChaseLevDeque elt -> IO Bool
nullQ CLD{top,bottom} = do
  b   <- readIORef bottom
  t   <- readIORef top  
--  return (b == t)
  let size = b - t  
  return (size <= 0)

-- | For a work-stealing queue `pushL` is the ``local'' push.  Thus
--   only a single thread should perform this operation.
pushL :: ChaseLevDeque a -> a  -> IO ()
-- pushL :: Show a => ChaseLevDeque a -> a  -> IO ()
pushL CLD{top,bottom,activeArr} obj = tryit "pushL" $ do
  b   <- readIORef bottom
  t   <- readIORef top
  arr <- readIORef activeArr
  let len = MV.length arr 
      size = b - t

--  when (size < 0) $ error$ "pushL: INVARIANT BREAKAGE - bottom, top: "++ show (b,t)

  arr' <- if (size >= len - 1) then do 
--            arr' <- gro arr (len + len) -- Double in size.
            arr' <- growCirc t b arr -- Double in size.

--            putStrLn$ "GREW IT: " ++ show arr'

            unless (MV.length arr  == len && 
		    MV.length arr' == (2*len))
              (error (printf "Contract violation! Grow didn't really grow the array!  Expected size %d, got %d\n" (2*len) (MV.length arr')))

            -- Only a single thread will do this:
	    writeIORef activeArr arr'
            return arr'
          else return arr

  -- when (b >= MV.length arr')$ 
  --   printf "HUH, bottom is off the end... out of date?, bottom = %d, old size %d (len=%d), new size %d\n" 
  -- 	   b (MV.length arr) len (MV.length arr')
  let newlen = MV.length arr'
--  wr arr' (b `mod` newlen)  obj
  putCirc arr' b obj
  writeIORef bottom (b+1)
  return ()

-- | This is the steal operation.  Multiple threads may concurrently
-- attempt steals from the same thread.
tryPopR :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopR CLD{top,bottom,activeArr} =  tryit "tryPopR" $ do
  t   <- readIORef top  
  b   <- readIORef bottom
  arr <- readIORef activeArr

--  when (b < t) $ error$ "tryPopR: INVARIANT BREAKAGE - bottom < top: "++ show (b,t)

  let size = b - t
  if size <= 0 then 
    return Nothing
   else do 
--    obj   <- rd arr (t `mod` MV.length arr)
    obj   <- getCirc  arr t
    (b,_) <- casIORef top t (t+1) 
    if b then 
      return (Just obj)
     else 
      return Nothing -- Someone beat us, abort

tryPopL  :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopL CLD{top,bottom,activeArr} = tryit "tryPopL" $ do
  b   <- readIORef bottom
  arr <- readIORef activeArr
  b   <- return (b - 1) -- shadowing
  writeIORef bottom b
  t   <- readIORef top    

--  when (b < t) $ error$ "tryPopL: INVARIANT BREAKAGE - bottom < top: "++ show (b,t)

  let size = b - t  
  if size < 0 then do
    writeIORef bottom t 
    return Nothing
   else do
--    obj <- rd arr (b `mod` MV.length arr)
    obj <- getCirc arr b
    if size > 0 then 
      return (Just obj)
     else do
      (b,_) <- casIORef top t (t+1)
      writeIORef bottom (t+1)
      if b then return$ Just obj
           else return$ Nothing 

--       return (if b 
-- 	      then Just obj 
-- 	      else Nothing)

------------------------------------------------------------
