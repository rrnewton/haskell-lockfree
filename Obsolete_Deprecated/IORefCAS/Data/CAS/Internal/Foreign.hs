{-# LANGUAGE FlexibleInstances, UndecidableInstances, MagicHash,
    TypeFamilies, MultiParamTypeClasses, OverlappingInstances, 
    BangPatterns, CPP #-}



-- | This is a version of CAS that works outside of Haskell by using
--   the FFI (and the GCC intrinsics-based 'Data.Bits.Atomic'.)

module Data.CAS.Internal.Foreign 
 ( 
   CASRef
   -- Plus instance...
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
import Unsafe.Coerce

import Data.CAS.Internal.Class

-- Convenient overlapping instances of CASable are possible at the at
-- the cost of a runtime dispatch on CASRef representations.  (Compile
-- time dispatch is not possible due to impossibility of overlapping
-- instances with associated type families.)
data CASRef a = 
   Frgn (Ptr a)
 | Hskl (ForeignPtr (StablePtr a))

--------------------------------------------------------------------------------
#if 1
-- | EXAMPLE SPECIALIZATION: a more efficient implementation for simple scalars.
-- 
--   Boilerplate TODO: We Should have one of these for all word-sized Scalar types.
-- 
instance CASable CASRef Word32 where 
-- -- We would LIKE to do this for everything in the Storable class:
-- instance (Storable a,  AtomicBits a) => CASable a where 
--
--  newtype CASRef a = Frgn (Ptr a)
--  newtype CASRef Word32 = Frgn (Ptr Word32)
  newCASable val = do 
    ptr <- malloc 
    poke ptr val
    return (Frgn ptr)

  writeCASable (Frgn ptr) val = poke ptr val

  readCASable (Frgn ptr) = peek ptr 

  {-# NOINLINE cas #-}
  cas (Frgn ptr) old new = do 
#  if 1
   -- I'm having problems with this version.  The ptrEq will report False even when the swap succeeds.
   -- I think the FFI unmarshalling the result ends up creating an extra copy.
--    orig <- compareAndSwap ptr old new 
--    printf "Completed swaps orig %d (%d) and old %d (%d)\n" orig (unsafeName orig) old (unsafeName old)
--    return (ptrEq orig old, orig)

   -- BUT, since it's a Word32 it is ok NOT to use pointer equality here.
   orig <- compareAndSwap ptr old new 
   return (orig == old, orig)

#  else
   -- ERROR: Trying this incorrect HACK version for a moment:
   -- This version will allow a return value of (False,old)
   snap <- peek ptr
   b <- compareAndSwapBool ptr old new 
   if b 
    then return (True, old)
    else return (False, snap)
#  endif

#endif

--------------------------------------------------------------------------------
#if 0
-- | INEFFICIENT but safe implementation for arbitrary Haskell values.
--   This version uses StablePtr's to store Haskell values in foreign storage.
-- 
-- This should NOT be useful for implementing efficient data
-- structures because it itself depends on concurrent access to
-- the GHC runtimes table of pinned StablePtr values.
instance CASable CASRef a where 
--  newtype CASRef a = Hskl (StablePtr a)

  newCASable val = do 
    -- Here we create a storage cell outside the Haskel heap which in
    -- turn contains a pointer back into the Haskell heap.
    p   <- newStablePtr val
--    mem <- malloc
--    poke mem p 
--    fp <- FC.newForeignPtr (castPtr$ castStablePtrToPtr p) (freeStablePtr p)    

    -- Here we assume that when we let go of the reference that we
    -- free whatever StablePtr is contained in it at the time.
    -- fp <- FC.newForeignPtr mem $ 
          -- There should be no races for this finalizer because all
          -- Haskell threads have let go of the foreign pointer:
--          do curp <- withForeignPtr fp peek 
--	     freeStablePtr curp
    fp <- mallocForeignPtr
    withForeignPtr fp (`poke` p)
    FC.addForeignPtrFinalizer fp $
         do putStrLn$ "EXPECTATION INVALIDATED: CURRENTLY THIS SHOULD NEVER HAPPEN BECAUSE THE FINALIZER KEEPS IT ALIVE!"
	    -- Todo... weak pointer here.
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
       
#endif


----------------------------------------------------------------------------------------------------
-- Helpers:
