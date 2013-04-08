

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

-----------------

AHA!  It works when I build like this:

   cabal install --disable-library-profiling --disable-documentation
   
But NOT with a vanially cabal install (profiling and documentation).
As expected, it is the profiling that makes the difference.
Note that I am NOT building the test with profiling.  The profiling
version should be IRRELEVANT.  Why does this cause the segfault?

Well, if it is cabal installed WITH profiling, we can change the
error, at least, by changing how Test.exe is built.  Building with no
options (without -threaded -O2) and we see a false positive with the
casArrayElem, then a segfault on the ticket-test:

    Perform a CAS within a MutableArray#
    (Poking at array was ok: 33)
      1st try should succeed: (False,4377552852)
    2nd should fail: (False,4377552852)
    Printing array:
      33  33  33  33  33
    Done.

      casmutarray1: [OK]

    Using new 'ticket' based compare and swap:
    YAY, read the IORef, ticket 4382124888
    Test.exe: internal error: MUT_VAR_DIRTY object entered!
	(GHC version 7.6.2 for x86_64_apple_darwin)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

What if we actually DO build with profiling?

    ghc  -prof -fforce-recomp Test.hs -o Test.exe
    
    Test.hs:39:10:
	Dynamic linking required, but this is a non-standard build (eg. prof).
	You need to build the program twice: once the normal way, and then
	in the desired way using -osuf to set the object file suffix.    

Oops, I have to disable the use of test-framework-th and template
haskell it seems.  I did that, and it builds and DOESN'T SEGFAULT. 
So it's building WITHOUT profiling after installing with profiling
that breaks!!


