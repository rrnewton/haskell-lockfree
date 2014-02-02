

-- | Integer counters providing thread-safe, lock-free mutation functions.
--
--   While this package provides multiple implementations, this module will always
--   expose the default (best) implementation.  Atomic counters are represented by a
--   single memory location, such that built-in processor instructions are sufficient
--   to perform fetch-and-add or compare-and-swap.
-- 
--   Remember, contention on such counters should still be minimized!

module Data.Atomics.Counter
       -- Reexport to get all the docs.
       (
         -- * Type of counters of counters and tickets
         AtomicCounter, 
         
         -- * Creating counters
         newCounter, 

         -- * Tickets, used for compare-and-swap         
         -- | See the documentation for "Data.Atomics" for more explanation of the
         -- ticket abstraction.  The same ideas apply here for counters as for
         -- general mutable locations (IORefs).
         CTicket, peekCTicket,

         -- * Atomic memory operations
         casCounter, incrCounter, incrCounter_,
                                  
         -- * Non-atomic operations
         readCounter, readCounterForCAS,
         writeCounter
         )
 where
-- This module reexports the default implementation of atomic counters:
import Data.Atomics.Counter.Unboxed
