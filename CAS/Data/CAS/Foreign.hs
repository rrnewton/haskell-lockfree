{-# LANGUAGE FlexibleInstances, UndecidableInstances, 
    MagicHash, TypeFamilies
 #-}


-- | This is a version of CAS that works outside of Haskell by using
--   the FFI (and the GCC intrinsics-based 'Data.Bits.Atomic'.)

module Data.CAS.Foreign 
 ( 
   CASable(..), CASref
--   casIORef, ptrEq 
 )
 where 

import Control.Monad
import Data.Bits.Atomic
import Data.IORef
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr
import Foreign.Marshal.Alloc (malloc)
import qualified Foreign.Concurrent as FC

import Text.Printf
import GHC.Exts (Int(I#))
import GHC.Prim (reallyUnsafePtrEquality#)
import Unsafe.Coerce

ptrEq :: a -> a -> Bool
ptrEq x y = I# (reallyUnsafePtrEquality# x y) == 1

-- Convenient overlapping instances at the cost of a runtime dispatch.
-- (Compile time dispatch is not possible due to impossibility of
--  overlapping instances with associated type families.)
data CASref a = 
   Frgn (Ptr a)
 | Hskl (ForeignPtr (StablePtr a))

class CASable a where 
--  data CASref a
  mkCASable    :: a -> IO (CASref a)
  readCASable  :: CASref a -> IO a 
  writeCASable :: CASref a -> a -> IO ()
  cas          :: CASref a -> a -> a -> IO (Bool,a)

-- instance (Storable a,  AtomicBits a) => CASable a where 
instance CASable Word32 where 
--  newtype CASref a = Frgn (Ptr a)
--  newtype CASref Word32 = Frgn (Ptr Word32)
  mkCASable val = do 
    ptr <- malloc 
    poke ptr val
    return (Frgn ptr)

  writeCASable (Frgn ptr) val = poke ptr val

  readCASable (Frgn ptr) = peek ptr 

  cas (Frgn ptr) old new = do 
    orig <- compareAndSwap ptr old new 
    return (ptrEq orig old, orig)


-- INEFFICENT but safe implementation.
-- 
-- This should not be useful for implementing efficient data
-- strcuctures because it itself dependends on concurrent access to
-- the GHC runtimes table of pinned StablePtr values.
instance CASable a where 
--  newtype CASref a = Hskl (StablePtr a)

  mkCASable val = do 
    -- Here we create a storage cell outside the Haskel heap which in
    -- turn contains a pointer back into the Haskell heap.
    p   <- newStablePtr val
--    mem <- malloc
--    poke mem p 
--    fp <- FC.newForeignPtr (castPtr$ castStablePtrToPtr p) (freeStablePtr p)    

    -- Here we assume that when we let go of the reference that we
    -- free whatever StablePtr is contained in it at the time.
    -- fp <- FC.newForeignPtr mem $ 
          -- There should be no races for this finalizer becuase all
          -- Haskell threads have let go of the foreign pointer:
--          do curp <- withForeignPtr fp peek 
--	     freeStablePtr curp
    fp <- mallocForeignPtr
    withForeignPtr fp (`poke` p)
    FC.addForeignPtrFinalizer fp $
         do putStrLn$ "CURRENTLY THIS SHOULD NEVER HAPPEN BECAUSE THE FINALIZER KEEPS IT ALIVE!"
	    -- Todo... week pointer here.
            curp <- withForeignPtr fp peek 
	    freeStablePtr curp

    return (Hskl fp)

  readCASable (Hskl ptr) = withForeignPtr ptr (\p -> peek p >>= deRefStablePtr)

  -- We must use CAS for ALL writes to ensure that we issue
  -- freeStablePtr for every value that gets bumped out of the foreign
  -- storage cell.
  writeCASable c val = readCASable c >>= loop
   where
    -- Hard spin: TODO add some contention back-off.
    loop x = do (b,v) <- cas c x val
		unless b (loop v)

  cas c@(Hskl ptr) old new = withForeignPtr ptr $ \ rawP -> 
    -- TODO: if we add an AtomicBits instance for StablePtr we can avoid these unsafe coercions
    do 
       osp <- newStablePtr old
       nsp <- newStablePtr new
       let oldRawPtr = unsafeCoerce osp :: Word
	   castP     = castPtr rawP :: Ptr Word
       orig <- compareAndSwap castP oldRawPtr (unsafeCoerce nsp)
       let fired = orig == oldRawPtr
           -- Restore the value we got back to its real type:
	   orig' = if True then unsafeCoerce orig else osp

       -- FIXME There's a problem here.  What if we put the same
       -- object in multiple CASRef's?  newStablePtr seems to return
       -- the same thing if called multiple times.

       orig'' <- deRefStablePtr orig'
       when fired $ freeStablePtr orig'
       return (fired, orig'')
       
--    unsafeCoerce 

--      p <- newStablePtr val
--      withForeignPtr (`poke` p)

--      undefined
  

-- type CASRef a = IORef (StablePtr a)


--  newStablePtr old 
