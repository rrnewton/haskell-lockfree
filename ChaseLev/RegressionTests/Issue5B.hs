{-# LANGUAGE FlexibleInstances, NamedFieldPuns, CPP #-}

module RegressionTests.Issue5B (standalone_pushPop) where

import Control.Concurrent
import Data.IORef
import qualified Data.Concurrent.Deque.Class as PC

-- We DO observe the error with SEPLIB on, and NOT with it off.
-- That means for the time being I cannot reproduce this bug in a standalone file...
#define SEPLIB
#ifdef SEPLIB
import Data.Concurrent.Deque.ChaseLev (ChaseLevDeque, dbgInspectCLD)
#endif

import Data.Atomics

-- import Data.CAS (casIORef)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
-- import Text.Printf (printf)
import Control.Exception(catch, SomeException, throw, evaluate)
import Control.Monad (when, unless, forM_)
-- import Control.Monad.ST

import Data.Atomics (readArrayElem, readForCAS, casIORef, Ticket, peekTicket)
import System.Mem.StableName (makeStableName, hashStableName)
import GHC.IO (unsafePerformIO)
import Text.Printf (printf)

--------------------------------------------------------------------------------

standalone_pushPop :: IO ()
standalone_pushPop =
  triv =<< (PC.newQ :: IO (DebugDeque ChaseLevDeque a))           
 where   
   -- This is what's failing with the debug wrapper, WHY?
   -- triv :: PC.PopL d => d [Char] -> IO ()
   triv :: DebugDeque ChaseLevDeque [Char] -> IO ()
   triv q = do
     -- r <- newIORef
     -- writeIORef r "hi"
     -- (bl,tick) <- readForCAS r 
     -- casIORef r tick "there"
     -- case bl of
     --   True -> putStrLn "CAS succeeded. Test passed.\n"
     let val = "hi"
     PC.pushL q val
     printf "Done pushing single value... queue %x, val %x\n" (unsafeName q) (unsafeName val)
     x <- PC.tryPopL q
     y <- PC.tryPopL q
     z <- PC.tryPopL q

     s <- dbgInspectCLD (unwrapDebug q)
     printf "Queue after 3 pops:\n%s\n" s     
     case x of
       Just "hi" -> putStrLn "Got expected value.  Test passed.\n"
       Just x'   -> error$ "A single push/pop got the WRONG value back: "++show x'
       Nothing   -> error$"Even a single push/pop in isolation did not work!: "++show (x,y,z)

data DebugDeque d elt =
  DebugDeque {
    tidMarks :: (IORef (Maybe ThreadId), IORef (Maybe ThreadId)),
    unwrapDebug :: (d elt)
  }

instance PC.DequeClass d => PC.DequeClass (DebugDeque d) where 
  pushL (DebugDeque (ref,_) q) elt = do
    PC.pushL q elt

  tryPopR (DebugDeque (_,ref) q) = do
    PC.tryPopR q 

  newQ = do l <- newIORef Nothing
            r <- newIORef Nothing
            fmap (DebugDeque (l,r)) PC.newQ

  -- FIXME: What are the threadsafe rules for nullQ?
  nullQ (DebugDeque _ q) = PC.nullQ q
      
  leftThreadSafe  (DebugDeque _ q) = PC.leftThreadSafe q
  rightThreadSafe (DebugDeque _ q) = PC.rightThreadSafe q


instance PC.PopL d => PC.PopL (DebugDeque d) where 
  tryPopL (DebugDeque (ref,_) q) = do
    PC.tryPopL q 

--------------------------------------------------------------------------------

{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)

#ifndef SEPLIB
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
    top       :: {-# UNPACK #-} !(IORef Int)
  , bottom    :: {-# UNPACK #-} !(IORef Int)
    -- This is a circular array:
  , activeArr :: {-# UNPACK #-} !(IORef (MV.IOVector a))
  }

--------------------------------------------------------------------------------
-- Debugging mode.
#define DEBUG
-- define FAKECAS

{-# INLINE rd #-}
{-# INLINE wr #-}
{-# INLINE nu #-}
{-# INLINE cpy #-}
{-# INLINE slc #-}
#ifndef DEBUG
dbg = False
nu  = MV.unsafeNew
rd  = MV.unsafeRead
wr  = MV.unsafeWrite
slc = MV.unsafeSlice
cpy = MV.unsafeCopy
#else
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
  ----------------------------------------
  -- Easier version first:
  let len   = MV.length oldarr
      elems = end - strt

  putStrLn$ "Grow to size "++show (len+len)++", copying over "++show elems

  newarr <- if dbg then
               nu (len + len)
            else  -- Better errors:
                V.thaw $ V.generate (len+len) (\i -> error (" uninitialized element at position " ++ show i
							    ++" had only initialized "++show elems++" elems: "
							    ++show(strt`mod`(len+len),end`mod`(len+len))))
  -- Strictly matches what's in the paper:
  forM_ [strt..end - 1] $ \ind -> do 
    x <- getCirc oldarr ind 
    evaluate x
    putCirc newarr ind x
  return newarr
{-# INLINE growCirc #-}

getCirc arr ind   = rd arr (ind `mod` MV.length arr)
putCirc arr ind x = wr arr (ind `mod` MV.length arr) x
{-# INLINE getCirc #-}
{-# INLINE putCirc #-}

copyOffset :: MV.IOVector t -> MV.IOVector t -> Int -> Int -> Int -> IO ()
copyOffset from to iFrom iTo len =
  cpy (slc iTo len to)
      (slc iFrom len from)
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
pushL CLD{top,bottom,activeArr} obj = tryit "pushL" $ do
  b   <- readIORef bottom
  t   <- readIORef top
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
  writeIORef bottom (b+1)
  return ()

-- | This is the steal operation.  Multiple threads may concurrently
-- attempt steals from the same thread.
tryPopR :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopR CLD{top,bottom,activeArr} =  tryit "tryPopR" $ do
--  t   <- readIORef top
  tt  <- readForCAS top
  b   <- readIORef bottom
  arr <- readIORef activeArr
 -- when (dbg && b < t) $ error$ "tryPopR: INVARIANT BREAKAGE - bottom < top: "++ show (b,t)

  let t = peekTicket tt
      size = b - t
  if size <= 0 then 
    return Nothing
   else do 
    obj   <- getCirc  arr t
    (b,_) <- doCAS top tt (t+1)
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
  tt   <- readForCAS top    
--  when (dbg && b < t) $ error$ "tryPopL: INVARIANT BREAKAGE - bottom < top: "++ show (b,t)

  let t = peekTicket tt
      size = b - t 
  if size < 0 then do
    writeIORef bottom t 
    return Nothing
   else do
    obj <- getCirc arr b
    if size > 0 then 
      return (Just obj)
     else do
      (b,_) <- doCAS top tt (t+1)
      writeIORef bottom (t+1)
      if b then return$ Just obj
           else return$ Nothing 

------------------------------------------------------------

{-# INLINE doCAS #-}
#ifdef FAKECAS
doCAS = fakeCAS
#else 
doCAS = casIORef
#endif

{-# INLINE fakeCAS #-}
-- This approach for faking it requires proper equality, it doesn't use pointer
-- equality at all.  That makes it not a true substitute but useful for some
-- debugging.
fakeCAS :: Eq a => IORef a -> Ticket a -> a -> IO (Bool,a)
-- casIORef r !old !new =   
fakeCAS r oldT new = do
  let old = peekTicket oldT
  atomicModifyIORef r $ \val -> 
{-
    trace ("    DBG: INSIDE ATOMIC MODIFY, ptr eqs found/expected: " ++ 
	   show [ptrEq val old, ptrEq val old, ptrEq val old] ++ 
	   " ptr eq self: " ++ 
	   show [ptrEq val val, ptrEq old old] ++
	   " names: " ++ show (unsafeName old, unsafeName old, unsafeName val, unsafeName val)
	  ) $
-}
    if   (val == old)
    then (new, (True, val))
    else (val, (False,val))

#endif
