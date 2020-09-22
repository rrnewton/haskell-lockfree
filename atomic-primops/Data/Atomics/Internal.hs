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
    peekTicketA,
    seal,
    -- * Very unsafe; for testing only
    reallyUnsafeTicketEquality
   )
  where

import GHC.Exts (Int(I#), RealWorld, Int#, State#, MutableArray#, MutVar#,
                 reallyUnsafePtrEquality#, readArray#,
                 casArray#, casIntArray#, fetchAddIntArray#, readMutVar#, casMutVar#, lazy)

#ifdef DEBUG_ATOMICS
{-# NOINLINE readForCAS# #-}
{-# NOINLINE readArrayElem# #-}
{-# NOINLINE casMutVarTicketed# #-}
{-# NOINLINE casArrayTicketed# #-}
{-# NOINLINE peekTicket #-}
{-# NOINLINE peekTicketA #-}
{-# NOINLINE seal #-}
#else
{-# INLINE readForCAS# #-}
{-# INLINE readArrayElem# #-}
{-# INLINE casMutVarTicketed# #-}
{-# INLINE casArrayTicketed# #-}
{-# INLINE peekTicket #-}
{-# INLINE peekTicketA #-}
{-# INLINE seal #-}
#endif

--------------------------------------------------------------------------------
-- CAS and friends
--------------------------------------------------------------------------------

-- | Unsafe, machine-level atomic compare and swap on an element within an Array.
casArrayTicketed# :: forall a. MutableArray# RealWorld a -> Int# -> Ticket a -> Ticket a
          -> State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
-- WARNING: cast of a function -- need to verify these are safe or eta expand.
casArrayTicketed# arr i (Ticket old) (Ticket new) s =
  case casArray# arr i old new s of
    (# s', flag, a #) -> (# s', flag, Ticket a #)

-- | Ordinary processor load instruction (non-atomic, not implying any memory barriers).
readArrayElem# :: forall a . MutableArray# RealWorld a -> Int#
  -> State# RealWorld -> (# State# RealWorld, Ticket a #)
readArrayElem# arr i s =
  case readArray# arr i s of
    (# s', a #) -> (# s', Ticket a #)

-- | When performing compare-and-swaps, the /ticket/ encapsulates proof that a
-- thread observed a specific previous value of a mutable variable.  It is
-- provided in lieu of the "old" value to compare-and-swap.
--
-- Design note: `Ticket`s exist to hide objects from the GHC compiler, which
-- can normally perform many optimizations that change pointer equality.  A
-- Ticket, on the other hand, is a first-class object that can be handled by
-- the user, but will not have its pointer identity changed by compiler
-- optimizations (but will of course, change addresses during garbage
-- collection).
data Ticket a = Ticket a
-- If we allow tickets to be a pointer type, then the garbage collector will
-- update the pointer when the object moves.

-- | Wrap up a Haskell value in a ticket. This is not exposed "publicly" for
-- now.  Presently the idea is that you must read from the mutable data
-- structure itself to get a ticket.
seal :: a -> Ticket a
seal = Ticket

-- | Extract a usable Haskell value from a ticket. In many cases, it is better
-- to use 'peekTicketA', to ensure the ticket is unboxed. @peekTicket@
-- works fine in strict contexts, however.
peekTicket :: Ticket a -> a
-- We use 'lazy' to guarantee that GHC's strictness analysis won't
-- force the ticket contents too early if it sees that the result of
-- `peekTicket` is eventually forced.
peekTicket (Ticket a) = lazy a

-- | Extract a usable Haskell value from a ticket.
peekTicketA :: Applicative f => Ticket a -> f a
-- We use 'lazy' to guarantee that GHC's strictness analysis won't
-- force the ticket contents too early if it sees that the result of
-- `peekTicket#` is eventually forced.
peekTicketA (Ticket a) = pure (lazy a)

instance Show (Ticket a) where
  show _ = "<CAS_ticket>"

-- | Check whether the contents of two tickets are the
-- same pointer. This is used only for testing.
reallyUnsafeTicketEquality :: Ticket a -> Ticket a -> Bool
reallyUnsafeTicketEquality (Ticket x) (Ticket y) = I# (reallyUnsafePtrEquality# x y) == 1

--------------------------------------------------------------------------------

readForCAS# :: forall a. MutVar# RealWorld a ->
               State# RealWorld -> (# State# RealWorld, Ticket a #)
readForCAS# ref s = case readMutVar# ref s of
  (# s', a #) -> (# s', Ticket a #)


casMutVarTicketed# :: forall a. MutVar# RealWorld a -> Ticket a -> Ticket a ->
               State# RealWorld -> (# State# RealWorld, Int#, Ticket a #)
casMutVarTicketed# ref (Ticket old) (Ticket new) s =
  case casMutVar# ref old new s of
    (# s', flag, a #) -> (# s', flag, Ticket a #)
