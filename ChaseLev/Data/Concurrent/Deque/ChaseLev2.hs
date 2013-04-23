{-# LANGUAGE FlexibleInstances, NamedFieldPuns, CPP #-}

-- | Chase-Lev work stealing Deques
-- 
-- This implementation derives directly from the pseudocode in the 2005 SPAA paper:
--
--   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.1097&rep=rep1&type=pdf
--
module Data.Concurrent.Deque.ChaseLev2
  (
    -- The convention here is to directly provide the concrete
    -- operations as well as providing the class instances.
    ChaseLevDeque(), newQ, nullQ, pushL, tryPopL, tryPopR
  )
 where

import Data.IORef
import qualified Data.Concurrent.Deque.Class as PC

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
-- import Data.Vector.Unboxed.Mutable as V
-- import Data.Vector
import Text.Printf (printf)
import Control.Exception(catch, SomeException, throw, evaluate)
import Control.Monad (when, unless, forM_)
-- import Control.Monad.ST

-- import Data.CAS (casIORef)
import Data.Atomics (casIORef, readForCAS)

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

data ChaseLevDeque a = CLD {
    top       :: {-# UNPACK #-} !(IORef Int)
  , bottom    :: {-# UNPACK #-} !(IORef Int)
    -- This is a circular array:
  , activeArr :: {-# UNPACK #-} !(IORef (MV.IOVector a))
  }

-- create a new queue, first implementation will not support resizing
newQ :: IO (ChaseLevDeque a)
newQ = do
  -- initial size of 8
  v <- MV.new 8
  bRef <- newIORef 0
  tRef <- newIORef 0
  vRef <- newIORef v
  return$ CLD bRef tRef vRef

nullQ :: ChaseLevDeque a -> IO Bool
nullQ = CLD { top, bottom } = do
  b <- readIORef bottom
  t <- readIORef top
  let size = b - t
  return (size <= 0)

pushL :: ChaseLevDeque a -> a -> IO ()
pushL CLD { top, bottom, array } obj = do
  b <- readIORef bottom
  t <- readIORef top
  arr <- readIORef array
  let len = MV.length arr
    size = b - t
 
  -- at this point handle resizing --

  putCirc arr b obj
  writeIORef bottom (b+1)
  RETURN ()

tryPopR :: ChaseLevDeque a -> IO (Maybe a)
tryPopR CLD { top, bottom, array } = do
  (ticket,t) <- readForCAS top
  b <- readIORef bottom
  arr <- readIORef array
  let size = b - t
  if size <= 0 then
    return Nothing
  else do
    obj <- getCirc arr t
    (bool,_) <- casIORef top ticket (t+1)
    if bool
      Just obj
    else
      Nothing 

tryPopL :: ChaseLevDeque a -> IO (Maybe a)
tryPopL CLD { top, bottom, array } = do
  (ticket,t) <- readForCAS top
  b <- readIORef bottom
  arr <- readIORef array
  let size = b - t
  if size <= 0 then
    return Nothing
  else do
    obj <- getCirc arr b
    (bool,_) <- casIORef top ticket (t+1)
    
  

