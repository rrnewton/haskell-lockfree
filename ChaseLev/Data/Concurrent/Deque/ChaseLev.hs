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
-- import Data.Vector.Unboxed.Mutable as V
-- import Data.Vector
import Text.Printf (printf)
import Control.Exception(catch, SomeException, throw)
import Control.Monad (when, unless)

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
-- Circular array routines:


-- TODO: make a "grow" that uses memcpy.


--------------------------------------------------------------------------------
-- Debugging mode.

{-# INLINE rd #-}
{-# INLINE wr #-}
{-# INLINE gro #-}
#if 0
gro = unsafeGrow
rd  = MV.unsafeRead
wr  = unsafeWrite
#else
gro = MV.grow
rd  = MV.read
-- wr  = MV.write

-- Our own bounds checking:
wr v i x = 
  -- [2012.03.08] We are indeed seeing a off-by-one error:
  if i >= MV.length v
  then error (printf "ERROR: Out of bounds of top of vector index %d, vec length %d\n" i (MV.length v))
  else MV.write v i x 
#endif

#if 1 
tryit msg action = Control.Exception.catch action 
	                        (\e -> do putStrLn$ "ERROR inside "++msg++" "++ show e 
                                          throw (e::SomeException))
#else
{-# INLINE tryit #-}
tryit msg action = action
#endif

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
  return (b == t)
--  let size = b - t  
--  return (size <= 0)

-- | For a work-stealing queue `pushL` is the ``local'' push.  Thus
--   only a single thread should perform this operation.
pushL :: ChaseLevDeque a -> a  -> IO ()
pushL CLD{top,bottom,activeArr} obj = tryit "pushL" $ do
  b   <- readIORef bottom
  t   <- readIORef top
  arr <- readIORef activeArr
  let len = MV.length arr 
      size = b - t

  -- when (len >= 209000)$ putStrLn$ "Big vector, pushL, size = "++show size

  arr' <- if (size >= len - 1) then do 
            arr' <- gro arr (len + len) -- Double in size.

            unless (MV.length arr  == len && 
		    MV.length arr' == (2*len))
              (error (printf "Contract violation! Grow didn't really grow the array!  Expected size %d, got %d\n" (2*len) (MV.length arr')))

            -- Only a single thread will do this:
	    writeIORef activeArr arr'
            return arr'
          else return arr


  when (b >= MV.length arr')$ printf "HUH, bottom is off the end... out of date?, bottom = %d, old size %d (len=%d), new size %d\n" b (MV.length arr) len (MV.length arr')

  wr arr' b obj
  writeIORef bottom (b+1)
  return ()

-- | This is the steal operation.  Multiple threads may concurrently
-- attempt steals from the same thread.
tryPopR :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopR CLD{top,bottom,activeArr} =  tryit "tryPopR" $ do
  t   <- readIORef top  
  b   <- readIORef bottom
  arr <- readIORef activeArr
  let size = b - t
  if size <= 0 then 
    return Nothing
   else do 
    obj <- rd arr t 
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
  let size = b - t  
  if size < 0 then do
    writeIORef bottom t 
    return Nothing
   else do
    obj <- rd arr b
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
