

[2013.04.07] {Figuring out install methods, linking, and segfaults}

Ok, here's the weird state of affairs at the moment.

 * Mac os, build test in-place compiling directly with Data.Atomics -- WORKS
 * mac os, cabal install then build test                            -- SEGFAULTS
 * linux, build test in-place compiling directly with Data.Atomics -- Fails link, no "cas".
 * linux, cabal install then build test            -- WORKS (but I thought segfaulted earlier?

Weird, right?  Well, I'm not surprised by the failed link, because
right now the foreign primops are trying to use a function defined in
SMP.h inside the GHC RTS.  That may be illegitemate.  (But at the same
time it is EXTERN_INLINE so shouldn't it just inline it before link
time?)

Note that SMP.h is included by Stg.h which is included by Rts.h.
Rts.h is included all over the place, but not directly by Cmm.h.

Anyway, putting in some extra printfs verifies that in the segfaulting
case it is returning a ticket of "zero" and in fact it is also
returning zero as a Haskell pointer because valgrind shows the
segfault as attempting to reference 0x0.  Just to confirm, I see
similar behavior if I just hack the CMM to return an invalid pointer:

    stg_readMutVar2zh {
      RET_NP(19, 19);
    }

In fact, with the segfaulting Mac OS config... if I use the above,
hacked stg_readMutVar2zh, I DO see it receives a ticket 19.  So it IS
calling the correct code.

WAIT!  There's not much we can infer from the previous.  Because, the
failure is odd and transient!  Once I restored stg_readMutVar2zh to
the original, it is now passing in BOTH compile methods on Mac OS.
Further, it will happily run hundreds of time without segfaulting.  So
it appears to be compile time nondeterminism rather than runtime ;-).


