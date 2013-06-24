

[2013.04.05] {A note on the ChaseLev crashes}
=============================================

On the Mine machines this will consistently get an uninitialized
error:

   time NUMELEMS=5000000 ./dist/build/test-chaselev-deque/test-chaselev-deque -tOneBot +RTS -N4

[2013.05.16] {Was being very silly before.}
===========================================

That "one bottle neck" test should NOT run on chase lev.  It has
multiple threads WRITING to the left end.

I am now seeing some failures on the random work stealing test now,
however, in this case some elements seem to be getting lost, but the
test itself may be wrong of course!

       ===============================================
	*** Iteration 6, executing command:  <./dist/build/test-chaselev-deque/test-chaselev-deque +RTS -N4 -RTS -t work_stealing>
       =================================================

    :work-stealing-deque-tests:
      [Warning]: Using environment variable NUMELEMS = 500000
    Grow to size 64, copying over 31
    Grow to size 64, copying over 31
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 62499750000
     but got: 62499749999

Uh oh, perhaps more worrying I just got this result where the size is TOO BIG:

      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 62499750000
     but got: 62499887547


[2013.05.17] {Back to debugging}
================================

Ok, I replaced the CAS' in ChaseLev.hs with atomicModifyIORef.  I
still see a failure with too big a sum:

    Checking that queue is finally null...
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 62499750000
     but got: 62499933370

And too little:

    expected: 62499750000
     but got: 62499749987

Let's audit the test some more...


[2013.06.01] {Back to it, with new debugging wrapper}
-----------------------------------------------------

Right now if I turn on the debugging wrapper (+realCAS), here's what happens:

    :ChaseLev(DbgWrapper):work-stealing-deque-tests:
    Grow to size 64, copying over 31
    Grow to size 128, copying over 63
    Grow to size 64, copying over 31
    Grow to size 128, copying over 63
    Grow to size 256, copying over 127
    Grow to size 256, copying over 127
    Grow to size 512, copying over 255
    Grow to size 512, copying over 255
    Grow to size 1024, copying over 511
    Grow to size 2048, copying over 1023
    Grow to size 1024, copying over 511
      :test_random_work_stealing: [Running]

      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 62499750000
     but got: 62252663774

The queues grow more than in normal runs.  That's fine, of course it
would change timing.

If I hop over to the debug branch, and then try the wrapper +
fakeCAS... what then?  Well, it's a LOT slower, but I still get this:

      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 62499750000
     but got: 62499749993

And if I crank the debug level up then I can't reproduce it.
If I selectively, turn on just one debug message I can get a little
more info while still failing:

    Final sum: 62461617055, producer/consumer/leftover sums: ([31240128173,31221488882],[0,0],[0,0])
      :test_random_work_stealing: [Running]

      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 62499750000
     but got: 62461617055
     

Interesting... the consumers were not successful at all...
(Eek, now it's got a build where it's failing ALL the time.)
[Oh wait... that's just a completely busted build.. Must be the profiling thing.
I seem to have situations where a cabal clean is necessary.  Not hermetic.]

Ugh, right now I'm seeing total failure of CAS even if I clean and
rebuild AtomicPrimops, AbstractDeque, and ChaseLev... what gives? 

Oh wait, this time it's NOT just the profiling.  The debug wrapper is
causing failures (how!?).  

    :ChaseLev:work-stealing-deque-tests:
      :test_ws_triv1: [OK]
      :test_ws_triv2: [OK]
    :ChaseLev(DbgWrapper):work-stealing-deque-tests:
      :test_ws_triv1: [Failed]
    ERROR: user error (Pattern match failure in do expression at Data/Concurrent/Deque/Tests.hs:334:3-8)
      :test_ws_triv2: [Failed]

In particular a push followed by an immediate tryPop is failing... 
That blows my mind because the debug wrapper is adding very little.
I already disabled the thread-marking so, basically, it is just eta expansion.

[2013.06.04] {Continued debugging}
----------------------------------

Ok, the extreme form of failure above... with triv1/triv2 failing.
That does NOT happen with fakeCAS.  So this is a nice small reproducer.

[2013.06.24] {Reference implementation does use leftover bucket}
----------------------------------------------------------------

By the way, the reference implementation passes all these tests with a
variety of different thread settings.  Also it DOES sometimes have
some numbers fall into the "leftover" bucket:

    Final sum: 4999950000, producer/consumer/leftover sums: ([3537163301],[1459399490],[3387209])

[2013.06.24] {back to eternal debugging}
----------------------------------------

What kinds of invariants should we start to see?  For one, I assert we
should NOT see this failure:

    Warning: Failed to pop 5000 times consecutively

Unless triv1 & triv2 also fail (i.e. CAS is completely broken because
of the old cabal/profiling bug or the new debug-wrapper oddity).  I
don't think I've invalidated that hypothesis yet.

I factored out the simple failure as a standalone test (see Issue #5
on this repo).  I need to go through the core to see what is
triggering it.  To get started, here are the relative sizes:

    O0_dump:
    total 135168
    -rw-r----- 1 rrnewton admin 31992 Jun 24 15:39 Issue5.dump-asm
    -rw-r----- 1 rrnewton admin 31560 Jun 24 15:39 Issue5.dump-cmm
    -rw-r----- 1 rrnewton admin  7340 Jun 24 15:39 Issue5.dump-ds
    -rw-r----- 1 rrnewton admin 36740 Jun 24 15:39 Issue5.dump-opt-cmm
    -rw-r----- 1 rrnewton admin  7283 Jun 24 15:39 Issue5.dump-simpl
    -rw-r----- 1 rrnewton admin   130 Jun 24 15:39 Issue5.dump-simpl-stats
    -rw-r----- 1 rrnewton admin 11912 Jun 24 15:39 Issue5.dump-stg

    O1_dump:
    total 208896
    -rw-r----- 1 rrnewton admin 41159 Jun 24 15:37 Issue5.dump-asm
    -rw-r----- 1 rrnewton admin 38537 Jun 24 15:37 Issue5.dump-cmm
    -rw-r----- 1 rrnewton admin  7340 Jun 24 15:37 Issue5.dump-ds
    -rw-r----- 1 rrnewton admin 44177 Jun 24 15:37 Issue5.dump-opt-cmm
    -rw-r----- 1 rrnewton admin 36091 Jun 24 15:37 Issue5.dump-simpl
    -rw-r----- 1 rrnewton admin  4210 Jun 24 15:37 Issue5.dump-simpl-stats
    -rw-r----- 1 rrnewton admin 22054 Jun 24 15:37 Issue5.dump-stg

The optimized one is bigger, likely as a result of link time inlining
from the existing libs.


