{-# LANGUAGE CPP, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides only the raw primops (and necessary types) for atomic
-- operations.  
module Data.Atomics.Internal 
   (
    casIntArray#, fetchAddIntArray#, 
    readForCAS#, casMutVarTicketed#, casArrayTicketed#, 
    Ticket (..),
    seal,
    peekTicket#,
    -- * Very unsafe, not to be used
    ptrEq,
    reallyUnsafeTicketEquality
   )
  where 

import GHC.Base (Int(I#))
import GHC.Exts (RealWorld, Int#, State#, MutableArray#, MutVar#,
                 reallyUnsafePtrEquality#, lazy, readMutVar#)

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (casArray#, casIntArray#, fetchAddIntArray#,
                 casMutVar#)
#else
import GHC.Exts (MutableByteArray#, unsafeCoerce#)
#endif    

#ifdef DEBUG_ATOMICS
{-# NOINLINE seal #-}
{-# NOINLINE readForCAS# #-}
{-# NOINLINE casMutVarTicketed# #-}
{-# NOINLINE casArrayTicketed# #-}
#else
-- {-# INLINE casMutVarTicketed# #-}
{-# INLINE casArrayTicketed# #-}
-- I *think* inlining may be ok here as long as casting happens on the arrow types:
#endif

--------------------------------------------------------------------------------
-- CAS and friends
--------------------------------------------------------------------------------

-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
casArrayTicketed# :: forall a. MutableArray# RealWorld a -> Int# -> Ticket a -> Ticket a 
          -> State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
#if MIN_VERSION_base(4,7,0)
   -- In GHC 7.8 onward we just want to expose the existing primop with a different type:
casArrayTicketed# mary i (Ticket a) (Ticket b) s =
  case casArray# mary i a b s of
    (# s', p, c #) -> (# s', p, Ticket c #)
#else
casArrayTicketed# mary i (Ticket a) (Ticket b) s =
  case unsafeCoerce# casArrayTypeErased# mary i a b s of
    (# s', p, c #) -> (# s', p, Ticket c #)
#endif
    
-- | When performing compare-and-swaps, the /ticket/ encapsulates proof
-- that a thread observed a specific previous value of a mutable
-- variable.  It is provided in lieu of the "old" value to
-- compare-and-swap.
--
-- Design note: `Ticket`s exist to hide objects from the GHC compiler, which
-- can normally perform many optimizations that change pointer equality.  A Ticket,
-- on the other hand, is a first-class object that can be handled by the user,
-- but will not have its identity changed by compiler optimizations
-- (but will of course, change addresses during garbage collection).
data Ticket a = Ticket a
-- If we allow tickets to be a pointer type, then the garbage collector will update
-- the pointer when the object moves.

-- The way we prevent GHC from messing with tickets is very simple: every time
-- we expose the contents of a ticket to user code, we wrap those contents with
-- the magical 'lazy' function. That should prevent GHC from ever unboxing
-- the contents. Indeed, 'lazy' doesn't inline until Core Prep, by which time
-- virtually all optimization is complete, so this should hide tickets very
-- effectively. The 'Ticket' constructor itself (which is only exposed through
-- this internal module) acts as a shield preventing user code from
-- accidentally breaking this rule.

-- Not exposing this from Data.Atomics for now. Presently the idea is that
-- you must read from the mutable data structure itself to get a ticket.
seal :: a -> Ticket a
seal = Ticket

-- | Extract a value from a ticket without forcing it.
peekTicket# :: Ticket a -> (# a #)
peekTicket# (Ticket a) = (# lazy a #)

instance Show (Ticket a) where
  show _ = "<CAS_ticket>"

-- Do we really want to force the values here?
-- Does that actually help us?
ptrEq :: a -> a -> Bool
ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1

-- | Check whether the contents of two tickets are the
-- same pointer. This is used only for testing.
reallyUnsafeTicketEquality :: Ticket a -> Ticket a -> Bool
reallyUnsafeTicketEquality (Ticket t1) (Ticket t2) =
  ptrEq (lazy t1) (lazy t2)

--------------------------------------------------------------------------------

readForCAS# :: forall a. MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, Ticket a #)
readForCAS# mv s =
  case readMutVar# mv s of
    (# s', a #) -> (# s', Ticket a #)

casMutVarTicketed# :: forall a. MutVar# RealWorld a -> Ticket a -> Ticket a ->
               State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
#if MIN_VERSION_base(4,7,0) 
casMutVarTicketed# mv (Ticket old) (Ticket new) s =
  case casMutVar# mv old new s of
    (# s', b, a #) -> (# s', b, Ticket a #)
#else
casMutVarTicketed# mv (Ticket old) (Ticket new) s =
  case unsafeCoerce# casMutVar_TypeErased# mv (Ticket old) (Ticket new) s of
    (# s', b, a #) -> (# s', b, Ticket a #)
#endif

--------------------------------------------------------------------------------
-- Type-erased versions that call the raw foreign primops:
--------------------------------------------------------------------------------
-- Due to limitations of the "foreign import prim" mechanism, we can't use the
-- polymorphic signature for the below functions.  So we lie to the type system
-- instead.

#if MIN_VERSION_base(4,7,0) 
#else

foreign import prim "stg_casArrayzh" casArrayTypeErased#
  :: MutableArray# RealWorld () -> Int# -> Any () -> Any () -> 
     State# RealWorld  -> (# State# RealWorld, Int#, Any () #) 
--   out_of_line = True
--   has_side_effects = True

-- | This alternate version of casMutVar returns an opaque "ticket" for
--   future CAS operations.
foreign import prim "stg_casMutVar2zh" casMutVar_TypeErased#
  :: MutVar# RealWorld () -> Any () -> Any () ->
     State# RealWorld -> (# State# RealWorld, Int#, Any () #)

-- foreign import prim "stg_readMutVar2zh" readMutVar_TypeErased#
--   :: MutVar# RealWorld () -> 
--      State# RealWorld -> (# State# RealWorld, Any () #)
  -- with has_side_effects = True
  --      commutable = False

foreign import prim "stg_casByteArrayIntzh" casIntArray#
  :: MutableByteArray# s -> Int# -> Int# -> Int# ->
     State# s -> (# State# s, Int# #) 

foreign import prim "stg_fetchAddByteArrayIntzh" fetchAddIntArray#
  :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #) 

#endif
