

[2013.04.07] {Figuring out install methods, linking, and segfaults}
-------------------------------------------------------------------

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
   
But NOT with a vanilla cabal install (profiling and documentation).
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

[2013.04.08] {Further investigating}
------------------------------------

Ok, let's see what the chain of causation here is.  HOW is it actually
picking up anything from those profiling installs if profiling is
disabled in the final build-and-link?

Here are the sizes of the relevant binaries on a profile compile:

    Summing ./lib/atomic-primops-0.1.0.0/ghc-7.6.2/HSatomic-primops-0.1.0.0.o
	      44,392 bytes in 1 plain files.
    Summing ./lib/atomic-primops-0.1.0.0/ghc-7.6.2/libHSatomic-primops-0.1.0.0.a
	      58,808 bytes in 1 plain files.


The final link command issued to gcc says this:

    '-L/Users/rrnewton/.cabal/lib/atomic-primops-0.1.0.0/ghc-7.6.2' ...
    '-lHSatomic-primops-0.1.0.0'

There doesn't seem to be anything that would cause it to pull in the
"_p.a" version...  How about I manually delete the "*p_*" files and
see what happens?  Same behavior!  It is NOT using those files
directly.

And, indeed, if I try to actually build the profiling version of
Test.exe then it fails to find Data.Atomics (presumably based on
failing to find the .p_hi file.)

How about a binary difference in the NON-profiling binaries?  Here are
their sizes in the non-profiling (WORKING) build.  YES, there are
binary differences:

            44,336 bytes in 1 plain files.
            58,752 bytes in 1 plain files.
	    
How about with ghc-7.4.2?
-------------------------

 * Exact same pattern of behavior, segfaults with atomic-primops/prof Test.exe/noprof
 

 
[2013.04.08] {Also trying HPC mode}
-----------------------------------

Under ghc-7.6.2, and installing with --enable-library-coverage, then
building Test.exe with -fhpc, it works on my Mac.

Also, note that the cabal driven test-suite is not working right now
on Mac or Linux....  I think it worked some before I moved it to the
testing/ subdir.

Having a completely separate .cabal for testing/...  That works on Mac
and Linux subject to the above bug concerning profiling installs.

[2013.04.20] {Cabal-dev poor isolation}
---------------------------------------

I thought I could build different GHC versions in parallel (ghc-7.6.2,
ghc-7.4.2 etc).  But it looks like they interfere.  Further, even when
running serially I'm getting excessive rebuilding as I switch modes
(e.g. "make prof74" then "make prof76").  I need to add some isolation
of my own.


[2013.04.20] {Debugging}
------------------------

I fixed a major bug in the primop.  I also verified that GC is what is
causing all_hammer_one to observe fewer successes than ideal.

Now it passes all tests, *some* of the time.  Other times I see
segfaults on the all_hammer_one test with 10K iterations.  The 10K
iteration one regularly sees GC's happen, whereas the 1K doesn't.  Yet
it APPEARS that the segfault happens *before* the first GC (i.e. as
reported by "+RTS -T").  Is it GC itself that is crashing?

 [Nope ,this was the null pointer error - FIXED]

-----------------------

Ok, things are going pretty well.  I've been running many tests with
7.4 and 7.6 that are passing completely.  However, I just switched
focus back to the profiling version and got this:

    Installed testing-0.1.0.0
    ./cabal-dev/ghc-7.4.2_prof/bin/test-atomic-primops_ghc-7.4.2 +RTS -N -T -RTS
    test-atomic-primops_ghc-7.4.2: internal error: evacuate(static): strange closure type -385875961
	(GHC version 7.4.2 for x86_64_apple_darwin)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    make[1]: *** [runtest] Abort trap: 6

On another run I got this:

    test-atomic-primops_ghc-7.4.2: internal error: MUT_VAR_DIRTY object entered!
	(GHC version 7.4.2 for x86_64_apple_darwin)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

And on 7.6 I got this:

    /cabal-dev/ghc-7.6.2_prof/bin/test-atomic-primops_ghc-7.6.2 +RTS -N -T -RTS
    test-atomic-primops_ghc-7.6.2: internal error: MVAR object entered!
	(GHC version 7.6.2 for x86_64_apple_darwin)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    make[1]: *** [runtest] Abort trap: 6
    
Uh oh... it seems like there's been regression on the profiling version....

STRANGE -- it looks like it was based on extra double quotes in the args to GHC:

    cabal-dev install "-v --enable-library-profiling --ghc-options=-prof" -s cabal-dev/ghc-7.6.2_prof --disable-documentation --with-ghc=ghc-7.6.2 --program-suffix=_ghc-7.6.2 .
    
How did that cause it to compile and yet run with an internal failure?



[2013.07.18] {Timing atomic Counter ops}
================================================================================

The foreign/unboxed counter is the fasted for single thread repeated incr:

  Laptop, Macbook Retina  (4 physical core w/ hyperthreading):
  
    $ ./dist/build/test-atomic-primops/test-atomic-primops -j 1 +RTS -N4 -RTS -t single
  
    Timing readIORef/writeIORef on one thread
    SELFTIMED: 0.137 sec
    Final value: 10000000
    RAW_single_thread_repeat_incr: [OK]    

    Timing CAS increments on one thread without retries
    SELFTIMED: 0.220 sec
    Final value: 10000000
    CAS_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.510 sec
    CounterReference_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.187 sec
    CounterIORef_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.119 sec
    CounterForeign_single_thread_repeat_incr: [OK]

  Desktop, veronica westmere 3.1 ghz, 4 core no-HT:

    SELFTIMED: 0.137 sec
    Final value: 10000000
    Timing CAS increments on one thread without retries
    RAW_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.228 sec
    Final value: 10000000
    CAS_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.638 sec
    CounterReference_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.242 sec
    CounterIORef_single_thread_repeat_incr: [OK]
    
    SELFTIMED: 0.135 sec
    CounterForeign_single_thread_repeat_incr: [OK]

The numbers for boolean-flipping (nots) are different than adds:

    Timing readIORef/writeIORef on one thread
    SELFTIMED: 0.097 sec
    Timing CAS boolean flips on one thread without retries
    RAW_single_thread_repeat_flip: [OK]
    
    SELFTIMED: 0.136 sec
    Final value: True
    Timing readIORef/writeIORef on one thread
    CAS_single_thread_repeat_flip: [OK]


--------------------------------------------------

With max contention on four threads (all threads hammering counter),
the atomicModifyIORef version falls apart.  E.g. for 1M total
increments here are the numbers:

  Laptop, Macbook Retina  (4 physical core w/ hyperthreading):
    SELFTIMED: 45.768 sec
    CounterReference_concurrent_repeat_incr: [OK]
    
    SELFTIMED: 0.348 sec
    CounterIORef_concurrent_repeat_incr: [OK]

    SELFTIMED: 0.060 sec
    CounterForeign_concurrent_repeat_incr: [OK]

  Desktop, veronica westmere 3.1 ghz, 4 core no-HT:

    SELFTIMED: 14.260 sec
    CounterReference_concurrent_repeat_incr: [OK]
    
    SELFTIMED: 0.426 sec
    CounterIORef_concurrent_repeat_incr: [OK]
    
    SELFTIMED: 0.066 sec
    CounterForeign_concurrent_repeat_incr: [OK]
    
MASSIVE difference between the foreign fetchAndAdd version and the
atomicModifyIORef.  But the difference is MUCH worse on my laptop.
The laptop has hyperthreading, but in the benchmark we only use four
unpinned threads, not eight....


[2013.08.02] {Just observed a failure}
----------------------------------------

On machine basalt, ghc 7.6.3.  But is it reproducible?

    create_and_mutate_twice: [OK]
    n_threads_mutate: [Failed]
    Did the sum end up equal to 120?
    run_barriers: [OK]


[2014.01.31] {Working on debugging CAS problems wiht n_threads_mutate test}
---------------------------------------------------------------------------

Now n_threads_mutate is failing consistently.  It seems that I'm
getting false POSITIVES when attempting a CAS.

    1 0: Fail when putting 1, was already 1
    2 3 2: Fail when putting 2, was already 3
    4 5 6 6: Fail when putting 6, was already 6
    7 3: Fail when putting 4, was already 5
    8 9 10 11 12 13 14 15 15 16 14: Fail when putting 11, was already 16
    17 18 19 20 21 22 23 24 25 26 27 28 29 31 30 33 34 35 32 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 106 105 107 108 109 110 111 112 113 114 115 116 117 118 119
    n_threads_mutate: [Failed]
    Did the 120 threads CASing all succeed?
    expected: 120
     but got: 119

Here are the "successes":

    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 15 16 17 18 19 20 21 22 23 24
    25 26 27 28 29 31 30 33 34 35 32 36 37 38 39 40 41 42 43 44 45 46
    47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
    69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90
    91 92 93 94 95 96 97 98 99 100 101 102 103 104 106 105 107 108 109
    110 111 112 113 114 115 116 117 118 119

Notice that 15 occurs TWICE.  Two threads think that they successfully
incremented 14 into 15.

Could that somehow happen if there were two different (boxed) objects
representing 14?  


[2014.04.13] {On debug branch, working on Issue 28}
---------------------------------------------------

Ok, right now Issue28.hs does fail whether it is run as a standalone
executable or as part of the bigger test suite:

    cd atomic-primops/testing/
    cabal install --reinstall --with-ghc=ghc-7.8.2 .. . --enable-tests 
    ./dist/build/test-atomic-primops/test-atomic-primops -j1 -t stand

And taht is in the same batch of tests where SIMILAR tests such as
case_casTicket1 pass.  Next, I also copied the same Issue28.hs code
into Test.hs with the other tests.

    ./dist/build/test-atomic-primops/test-atomic-primops -j1 -t issue28_copied

This version fails in the same way -- the ticket gets corrupted.




[2014.04.13] {GHC panic}
------------------------

Just cherry-picked a patch from the debug to master branch.
Got this:

    $ ghc-7.8.2 -main-is Issue28.main -threaded -O2 Issue28.hs -fforce-recomp -rtsopts -with-rts
    opts=-N4 $GHCDUMP
    [1 of 1] Compiling Issue28          ( Issue28.hs, Issue28.o )
    ghc: panic! (the 'impossible' happened)
      (GHC version 7.8.2 for x86_64-apple-darwin):
            Iface Lint failure
        In interface for atomic-primops-0.6.0.4:Data.Atomics
        Unfolding of casIORef{v r5s}
          <no location info>: Warning:
              In the expression: casMutVar#{(w) v 94F} [gid[PrimOp]]
                                   @ Any{(w) tc 31N}
                                   @ Any{(w) tc 31N}
                                   (var{v a12G} [lid]
                                    `cast` ((MutVar#{(w) tc 32F}
                                               (UnivCo nominal RealWorld{(w) tc 31E} Any{(w) tc 31N})
                                               (UnivCo representational
                                                  a11{tv a12y} [tv] Any{(w) tc 31N}))_R
                                            :: MutVar#{(w) tc 32F}
                                                 RealWorld{(w) tc 31E} a11{tv a12y} [tv]
                                                 ~#
                                               MutVar#{(w) tc 32F} Any{(w) tc 31N} Any{(w) tc 31N}))
                                   (old{v a12A} [lid]
                                    `cast` (Any{(w) tc 31N}_R :: Any{(w) tc 31N} ~# Any{(w) tc 31N}))
                                   (new{v a12B} [lid]
                                    `cast` (UnivCo representational
                                              a11{tv a12y} [tv] (Ticket{tc r5g} a11{tv a12y} [tv])
                                            ; Any{(w) tc 31N}_R
                                            :: a11{tv a12y} [tv] ~# Any{(w) tc 31N}))
                                   (st{v a12I} [lid]
                                    `cast` ((State#{(w) tc 32q}
                                               (UnivCo nominal RealWorld{(w) tc 31E} Any{(w) tc 31N}))_R
                                            :: State#{(w) tc 32q} RealWorld{(w) tc 31E}
                                                 ~#
                                               State#{(w) tc 32q} Any{(w) tc 31N}))
              From-type of Cast differs from type of enclosed expression
              From-type: Any{(w) tc 31N}
              Type of enclosed expr: Ticket{tc r5g} a11{tv a12y} [tv]
              Actual enclosed expr: old{v a12A} [lid]
              Coercion used in cast: Any{(w) tc 31N}_R



