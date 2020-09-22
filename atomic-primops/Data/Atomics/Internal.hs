{-# LANGUAGE CPP, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#define CASTFUN

-- | This module provides only the raw primops (and necessary types) for atomic
-- operations.
module Data.Atomics.Internal
   (
    casIntArray#, fetchAddIntArray#,
    readForCAS#, casMutVarTicketed#, casArrayTicketed#,
    readArrayElem#,
    Ticket,
    peekTicket,
    seal,
    -- * Very unsafe; for testing only
    reallyUnsafeTicketEquality
   )
  where

import GHC.Exts (Int(I#), Any, RealWorld, Int#, State#, MutableArray#, MutVar#,
                 reallyUnsafePtrEquality#, readArray#,
                 casArray#, casIntArray#, fetchAddIntArray#, readMutVar#, casMutVar#, lazy)
import Data.Coerce (coerce)

#ifdef DEBUG_ATOMICS
{-# NOINLINE readForCAS# #-}
{-# NOINLINE readArrayElem# #-}
{-# NOINLINE casMutVarTicketed# #-}
{-# NOINLINE casArrayTicketed# #-}
{-# NOINLINE peekTicket #-}
{-# NOINLINE seal #-}
#else
{-# INLINE readForCAS# #-}
{-# INLINE readArrayElem# #-}
{-# INLINE casMutVarTicketed# #-}
{-# INLINE casArrayTicketed# #-}
{-# INLINE peekTicket #-}
{-# INLINE seal #-}
#endif

--------------------------------------------------------------------------------
-- CAS and friends
--------------------------------------------------------------------------------

-- | Unsafe, machine-level atomic compare and swap on an element within an Array.
casArrayTicketed# :: forall a. MutableArray# RealWorld a -> Int# -> Ticket a -> Ticket a
          -> State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
-- WARNING: cast of a function -- need to verify these are safe or eta expand.
casArrayTicketed# = coerce
  ( casArray#
      :: MutableArray# RealWorld a -> Int# -> a -> a
      -> State# RealWorld -> (# State# RealWorld, Int#, a #) )

-- | Ordinary processor load instruction (non-atomic, not implying any memory barriers).
readArrayElem# :: forall a . MutableArray# RealWorld a -> Int#
  -> State# RealWorld -> (# State# RealWorld, Ticket a #)
readArrayElem# = coerce
  ( readArray#
    :: MutableArray# RealWorld a -> Int#
      -> State# RealWorld -> (# State# RealWorld, a #) )

-- | When performing compare-and-swaps, the /ticket/ encapsulates proof that a
-- thread observed a specific previous value of a mutable variable.  It is
-- provided in lieu of the "old" value to compare-and-swap.
-- A ticket should never be forced using 'seq' or otherwise. Doing so may "go
-- back in time" and mess up the ticket from the moment of its creation
-- so operations on it are likely to fail.
--
-- Design note: `Ticket`s exist to hide objects from the GHC compiler, which
-- can normally perform many optimizations that change pointer equality.  A
-- Ticket, on the other hand, is a first-class object that can be handled by
-- the user, but will not have its pointer identity changed by compiler
-- optimizations (but will of course, change addresses during garbage
-- collection).
newtype Ticket a = Ticket a
-- If we allow tickets to be a pointer type, then the garbage collector will
-- update the pointer when the object moves.

-- | Wrap up a Haskell value in a ticket. This is not exposed "publicly" for
-- now.  Presently the idea is that you must read from the mutable data
-- structure itself to get a ticket.
seal :: a -> Ticket a
seal = Ticket

-- | Extract a usable Haskell value from a ticket.
peekTicket :: Ticket a -> a
-- We use 'lazy' to guarantee that GHC's strictness analysis won't
-- unbox the ticket if it sees that the result of `peekTicket`
-- is eventually forced.
peekTicket (Ticket a) = lazy a

instance Show (Ticket a) where
  show _ = "<CAS_ticket>"

-- | Check whether the contents of two tickets are the
-- same pointer. This is used only for testing.
reallyUnsafeTicketEquality :: Ticket a -> Ticket a -> Bool
reallyUnsafeTicketEquality x y = I# (reallyUnsafePtrEquality# x y) == 1
{-# NOINLINE reallyUnsafeTicketEquality #-}

--------------------------------------------------------------------------------

readForCAS# :: forall a. MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, Ticket a #)
readForCAS# = coerce
  ( readMutVar#
      :: MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, a #) )


casMutVarTicketed# :: forall a. MutVar# RealWorld a -> Ticket a -> Ticket a ->
               State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
casMutVarTicketed# = coerce
  ( casMutVar#
      :: MutVar# RealWorld a -> a -> a ->
               State# RealWorld -> (# State# RealWorld, Int#, a #) )
