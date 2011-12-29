{-# LANGUAGE FlexibleInstances, NamedFieldPuns #-}

-- | Chase-Lev work stealing Deques
-- 
-- This implementation derives directly from the pseudocode in the 2005 SPAA paper.
module Data.Concurrent.Deque.ChaseLev 
  (
    -- The convention here is to directly provide the concrete
    -- operations as well as providing the class instances.
    ChaseLevDeque(),
    newQ, nullQ, pushL, tryPopL, tryPopR
  )
 where

import Data.IORef
import qualified Data.Concurrent.Deque.Class as PC

import Data.CAS (casIORef)
import Data.Vector.Mutable as V
-- import Data.Vector.Unboxed.Mutable as V
-- import Data.Vector

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
  , activeArr :: {-# UNPACK #-} !(IORef (IOVector a))
  }

--------------------------------------------------------------------------------
-- Queue Operations

-- logInitialSize
newQ :: IO (ChaseLevDeque elt)
newQ = do
  v <- V.new 32 
  r1 <- newIORef 0
  r2 <- newIORef 0
  r3 <- newIORef v
  return$ CLD r1 r2 r3

nullQ :: ChaseLevDeque elt -> IO Bool
nullQ CLD{top,bottom} = do
  b   <- readIORef bottom
  t   <- readIORef top  
  let size = b - t  
  return (size <= 0)

-- | For a work-stealing queue `pushL` is the ``local'' push.  Thus
--   only a single thread should perform this operation.
pushL :: ChaseLevDeque a -> a  -> IO ()
pushL CLD{top,bottom,activeArr} obj =   do
  b   <- readIORef bottom
  t   <- readIORef top
  arr <- readIORef activeArr
  let len = V.length arr 
      size = b - t
  arr' <- if (size >= len - 1) then do 
            arr' <- unsafeGrow arr (len + len)
	    writeIORef activeArr arr'
            return arr'
          else return arr
  unsafeWrite arr' b obj
  writeIORef bottom (b+1)
  return ()

-- | This is the steal operation.  Multiple threads may concurrently
-- attempt steals from the same thread.
tryPopR :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopR CLD{top,bottom,activeArr} = do 
  t   <- readIORef top  
  b   <- readIORef bottom
  arr <- readIORef activeArr
  let size = b - t
  if size <= 0 then 
    return Nothing
  else do 
    obj <- V.unsafeRead arr t 
    (b,_) <- casIORef top t (t+1) 
    if b then 
      return (Just obj)
     else 
      return Nothing -- Someone beat us, abort

tryPopL  :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopL CLD{top,bottom,activeArr} = do 
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
    obj <- V.unsafeRead arr b
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

t0 = do
  q <- newQ
  pushL q "one" 
  pushL q "two" 
  pushL q "three" 
  pushL q "four" 
  sequence [tryPopR q, tryPopR q, 
	    tryPopL q, tryPopL q,
	    tryPopL q, tryPopR q ]

t1 = do
  q <- newQ
  pushL q "hi" 
  tryPopL q 
