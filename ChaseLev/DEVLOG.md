

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

[2013.06.24] {btw, Reference implementation does use leftover bucket}
---------------------------------------------------------------------

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

Here's something weird... changing Issue5.hs to Issue5B.hs and
inlining the definition of ChaseLev makes the problem GO AWAY.  Why
would that be?  That could only enable more inlining, right?

[2013.06.25] {Continued}
------------------------

Ok, now it is narrowed down to a situation where a simple CAS counter
bump for an IORef Int is failing.  Really, this should be switched for
some kind of unboxed int container (MutArray) but I would like to know
why this is failing in any case.

The ticketing approach SHOULD save us (Any Kind), but it's not.  This
is a situation (in tryPopL), where:

 * We are executing a readForCAS followed by a casIORef, presenting
   the ticket for the old value properly; leaving it abstract.
 * Stable names report that the old and new values were the same, even
   though CAS failed.
 * reallyUnsafePtrEquality# reports that the old and new values are
   the same, even though CAS failed. 

So here's the core that gets generated for the failing CAS.  You can
see how tickets are represented by unsafe coercions to Any.  But what
results is a complicated series of unsafe coercions that is tricky to
follow:

   False ->
     case ({__pkg_ccall_GC atomic-primops-0.1.0.2 stg_casMutVar2zh MutVar#
				RealWorld ()
			      -> Any (* -> *) ()
			      -> Any (* -> *) ()
			      -> State# RealWorld
			      -> (# State# RealWorld,
				    Int#,
				    Any (* -> *) () #)}_a1hq
	     (ww_s2ze
	      `cast` (MutVar# <RealWorld> (UnsafeCo Int ())
		      :: MutVar# RealWorld Int ~# MutVar# RealWorld ()))
	     (wild1_a1v5
	      `cast` (UnsafeCo Int (Ticket Int) ; UnsafeCo
						    (Ticket Int)
						    (Any (* -> *) ())
		      :: Int ~# Any (* -> *) ()))
	     ((I# (+# y_a1v7 1))
	      `cast` (UnsafeCo Int (Ticket Int) ; UnsafeCo
						    (Ticket Int)
						    (Any (* -> *) ())
		      :: Int ~# Any (* -> *) ()))
	     ipv8_X1R1)
	  `cast` ((# <State# RealWorld>,
		     <Int#>,
		     UnsafeCo (Any (* -> *) ()) (Ticket Int) #)
		  :: (# State# RealWorld, Int#, Any (* -> *) () #)
		       ~#
		     (# State# RealWorld, Int#, Ticket Int #))
     of _ { (# ipv10_a1ht, ipv11_a1hu, ipv12_a1hv #) ->

Here's a somewhat simpler version that results ifwe make casIORef
NOINLINE:

    False ->
      case ((casIORef
	       @ Int
	       ((STRef @ RealWorld @ Int ww_s2yd)
		`cast` (Sym <(NTCo:IORef)> <Int>
			:: STRef RealWorld Int ~# IORef Int))
	       (wild1_a1uC
		`cast` (UnsafeCo Int (Ticket Int) :: Int ~# Ticket Int))
	       (I# (+# y_a1uE 1)))
	    `cast` (<NTCo:IO <(Bool, Ticket Int)>>
		    :: IO (Bool, Ticket Int)
			 ~#
		       (State# RealWorld
			-> (# State# RealWorld, (Bool, Ticket Int) #))))
	     ipv8_X1Ql

In both cases you can see that the wild1_a1* arguments, i.e. the OLD
value presented as evidence, the thing which is SUPPOSED to be
abstract, is really passed in as a raw Int, and coerced at the last
moment to Ticket Int. (At least if I'm reading those UnsafeCo
constructors properly.)

But, this program should have NEVER had the handle on the Int for that
variable!  It should have only seen the "Ticket Int".

Where does that value come from?  Code above the previous snippet does
this to retrieve it:

         case ipv7_X1PZ
              `cast` (UnsafeCo (Ticket Int) Int :: Ticket Int ~# Int)
         of wild1_a1uC { I# y_a1uE ->

Which is part of this bigger piece, reading the value from the mutable var:

         case (readMutVar#
                 @ (Any *)
                 @ (Any *)
                 (ww_s2yd
                  `cast` (MutVar# (UnsafeCo RealWorld (Any *)) (UnsafeCo Int (Any *))
                          :: MutVar# RealWorld Int ~# MutVar# (Any *) (Any *)))
                 (s2#_a263
                  `cast` (State# (UnsafeCo RealWorld (Any *))
                          :: State# RealWorld ~# State# (Any *))))
              `cast` ((# State# (UnsafeCo (Any *) RealWorld),
                         UnsafeCo (Any *) (Ticket Int) #)
                      :: (# State# (Any *), Any * #)
                           ~#
                         (# State# RealWorld, Ticket Int #))
         of _ { (# ipv6_X1PX, ipv7_X1PZ #) ->
         case ipv5_X1PQ of _ { I# x_a1uA ->
         case ipv7_X1PZ
              `cast` (UnsafeCo (Ticket Int) Int :: Ticket Int ~# Int)
         of wild1_a1uC { I# y_a1uE ->

Ok, it's fair that we are unpacking wild1_a1uC... we need to do that
to get the value on which to perform "plus 1".  

But it's NOT good that we using wild1_a1uC (which is an Int) instead
of using ipv7_X1PZ (which is a Ticket Int) as the old value for the
CAS operation.  Whenever we use the "Int" GHC can play naughty tricks
-- the ticket did NOT stay abstract here.

But, as written, the result of readForCAS is fed directly into
casIORef.  So what optimization rule allowed it to short-circuit that,
peering through the code introduce by "readForCAS" + "peekTicket", and
seeing the raw, unticketed value?

Was it common-subexpression-elimination?  No... I don't see where.
That is, readForCAS# uses a coerce at the level of a function to
expose readMutVar# as a ticketed operation.  Then peekTicket uses a
coerce on the result to get back to an Int.

Maybe it's a fusion rule for coercions?  Short circuiting these
multiple coercions and allowing us to reference the source directly?

HYPOTHESIS 1: coercing arrow types is bad.
------------------------------------------
 
The theory here is that it doesn't establish a correct data-flow for
the optimizer to observe.  Let's try getting rid of these.  

Ok, fixing this by itself doesn't seem to work.  At the point of the
readMutVar# we get this code:

         case readMutVar# @ RealWorld @ Int ww_s2ye s2#_a263
         of _ { (# ipv6_a28F, ipv7_a28G #) ->
         case ipv5_X1PP of _ { I# x_a1uA ->
         case ipv7_a28G of wild1_a1uC { I# y_a1uE ->

And wild1_a1uC is still used at the casIORef, cast just at the last
moment from Int to Ticket Int, which doesn't help at all.

### What about ALSO making readForCAS NOINLINE?  

The result of that is interesting...

         case readForCAS# @ Int ww_s2yn s_a1LD
         of _ { (# ipv_a1LG, ipv1_a1LH #) ->
	 .....
         case ipv1_a1LH
              `cast` (UnsafeCo (Ticket Int) Int :: Ticket Int ~# Int)
         of wild1_a1uC { I# y_a1uE ->
         .....
                           case ((casIORef
                                    @ Int
                                    ((STRef @ RealWorld @ Int ww_s2yn)
                                     `cast` (Sym <(NTCo:IORef)> <Int>
                                             :: STRef RealWorld Int ~# IORef Int))
                                    (wild1_a1uC
                                     `cast` (UnsafeCo Int (Ticket Int) :: Int ~# Ticket Int))
                                    (I# (+# y_a1uE 1)))

So we only observe the Ticket version (ipv1) in this case, however, it
STILL uses wild1_a1uC, even though it must coerce it to a (Ticket
Int), rather than using ipv1_a1LH as the old value for the CAS.

So it would seem that we start out with this dataflow:

   readForCAS -> 
    ipv1 -> peekTicket -> wild1_a1uC
         |-> casIORef 
	
with the result (ipv1) used in two different places, both peekTicket
and casIORef.  Yet at some point the dataflow gets changed so the
result of peekTicket ALSO becomes the input to casIORef.  I.e.:

    ipv1 -> peekTicket -> wild1_a1uC -> casIORef 

How and why could that happen?  Again, is it special rules for coerce,
or a general optimization?

First, let's get a litle more specific about the above sketches of the
dataflow graph.  If we look at the desugared Haskell, *.dump-ds, we
see the following:
   
    (>>=
      @ IO
      $fMonadIO
      @ (Ticket Int)
      @ (Maybe elt_tt)
      (readForCAS @ Int top_aVg)
      (\ (tt_aVm :: Ticket Int) ->
         let {
	 t_aVn :: Int
	 [LclId]
	 t_aVn = peekTicket @ Int tt_aVm } in
      ..........
		    False ->
		      >>=
			@ IO
			$fMonadIO
			@ (Bool, Ticket Int)
			@ (Maybe elt_tt)
			(doCAS
			   @ Int
			   top_aVg
			   tt_aVm
			   (+ @ Int $fNumInt t_aVn (I# 1)))
      ......			   
	 ))

At this point you can confirm that we were right above.  tt_aVm flows
to both peekTicket and doCAS.  At what point do wwe then substitute
t_aVn for tt_aVm?  
   Well, no matter where it happens, it is fair to say that this
requires inlining peekTicket, which then exposes that it is nothing
but an unsafeCoerce.  

### Next, let's NOINLINE peekTicket.

That fixes issue5.


[2013.06.25] {Segfault!}
========================

     ./dist/build/test-chaselev-deque/test-chaselev-deque -t 'test_contention_free_parallel'
    Running with numElems 100000 and numAgents 1
    Use NUMELEMS and +RTS to control the size of this benchmark.
    Running on a machine with 8 hardware threads.
    Running all tests for these thread settings: [1,2,4,8,16]
    ............
    :N2_ChaseLev(DbgWrapper):work-stealing-deque-tests:

       [Setting # capabilities to 2 before test]
    Grow to size 64, copying over 31
    .Grow to size 128, copying over 63
    .Grow to size 256, copying over 127
    .Grow to size 512, copying over 255
    .Grow to size 1024, copying over 511
    Grow to size 2048, copying over 1023
    Grow to size 4096, copying over 2047
    Grow to size 8192, copying over 4095
    Grow to size 16384, copying over 8191

       [Setting # capabilities to 4 before test]

    Segmentation fault: 11


Ok, because of recent complications, I've had to exposesome more knobs
to be able to run a single stress test.  Here is one test:

    time NUMELEMS=5000000 NOWRAPPER=1 NUMTHREADS=4 ./dist/build/test-chaselev-deque/test-chaselev-deque -t 'test_contention_free_parallel'

    time DEBUG=1 NUMELEMS=5000000 NOWRAPPER=1 NUMTHREADS=4 ./dist/build/test-chaselev-deque/test-chaselev-deque -t 'test_contention_free_parallel'

This one seems to not have problems:
    
    NUMELEMS=5000000 NOWRAPPER=1 NUMTHREADS=4 rep 10 time ./dist/build/test-chaselev-deque/test-chaselev-deque -t 'test_contention_free_parallel'

But it's slow, it takes almost three seconds on my laptop to push 5M
elements (across four deques).  It seems to grow the arrays rather
large:

    Grow to size 2097152, copying over 1048575
    Grow to size 2097152, copying over 1048575
    
How about the other benchmarks?  The old "OneBottleNeck" test doesn't
work for work stealing, but test_random_work_stealing is one that
stresses all valid operations.  It doesn't grow the arrays NEARLY as
much, but it is significantly slower, taking 12-13 seconds on my
retina mbp to push 5M elements across either two or four threads.

Ack!  Further, we've still got bad sums, even though Issue5 is closed:

    time DEBUG=1 NUMELEMS=5000000 NOWRAPPER=1 NUMTHREADS=4 ./dist/build/test-chaselev-deque/test-chaselev-deque -t random_work_steal    
    .....
    Final sum: 6249998732279, producer/consumer/leftover sums: ([1806520075438,1742608637591],[1351485936177,1349384083073],[0,0])
    Checking that queue is finally null...
      :test_random_work_stealing: [Running]
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 6249997500000
     but got: 6249998732279

Actually, running it at 1M is enough to trigger the error:

    NUMELEMS=1000000 NOWRAPPER=1 NUMTHREADS=4 rep 10 ./dist/build/test-chaselev-deque/test-chaselev-deque -t random_work_steal
    
And, btw, it triggers when DEBUGCL is on too.

Added another hack (ONLYWRAPPER) so I can test the debug wrapper by
itself again.  It still gets bad sums without catching a violation of
the thread/queue contract:

    NUMELEMS=1000000 ONLYWRAPPER=1 NUMTHREADS=4 rep 10 ./dist/build/test-chaselev-deque/test-chaselev-deque -t random_work

### Pump up the NOINLINE's in Data.Atomics

I don't currently know how many functions have to be marked NOINLINE
to avoid problems analagous to the one exposed by "peekTicket".  If we
conservatively apply NOINLINE to ALL functions in Data.Atomics.hs,
what happens?

    DEBUG=1 NUMELEMS=1000000 NOWRAPPER=1 NUMTHREADS=4 rep 10 ./dist/build/test-chaselev-deque/test-chaselev-deque -t random_work

The short answer is that we still get those same failures:

    Final sum: 249999499999, producer/consumer/leftover sums: ([79457516832,80190885993],[45385333059,44965764115],[0,0])
      :test_random_work_stealing: [Running]
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 249999500000
     but got: 249999499999

Hmm, these answers, even if we pump the size up to 5M, are still
around 42 bits big.  I don't think any kind of overflow could possible
be occuring, given as we use 64 bit ints on a 64 bit machine.  But it
wouldn't hurt to make them explicitly 64 bit to make sure.. (or
Integer)... ok, did that.  No change as expected.

### Instrumenting test to return more info on #successes

Ok, in the current version of the random_work_stealing test, the
consumers will TRY (finite spinPop) the same number of times as the
producers push, and any failures must be made up for in the
"leftovers" phase.

    Final sum: 6249999558444, producer/consumer/leftover sums: 
      ([1938888882141,1925206902443],[(945156,1191595147306),(949088,1194308626554)],[(0,0),(0,0)])

Let's look a little deeper at the # of successful pops.  Ok this is
exactly what you might expect.  The total number of pops is off by
ONE:

    Final sum: 6249997583859, producer/consumer/leftover sums: ([(1510144,1924759993822),(1536889,1924135117200)],[(977451,1202425485357),(975517,1198676987480)],[(0,0),(0,0)])
    Total pop events: 5000001 should be 5000000
    Checking that queue is finally null...
      :test_random_work_stealing: [Running]
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 6249997500000
     but got: 6249997583859
     
In this case we can even say which element got duplicated!  It was
iteration 83,859.

Now, here's something odd.  When we end up with too small a sum, we
actually have the CORRECT number of pops: 

    Final sum: 6249997499996, producer/consumer/leftover sums: ([(1537725,1903359749484),(1555446,1947679153181)],[(955687,1201494171631),(951142,1197464425700)],[(0,0),(0,0)])
    Total pop events: 5000000 should be 5000000
    Checking that queue is finally null...
      :test_random_work_stealing: [Running]
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 6249997500000
     but got: 6249997499996
     
(1537725 + 1555446  + 955687 + 951142 = 5M)

So apparently we can have errors both with duplicated elements
and... what? aliased dropped and duplicated elements?  Corrupted
elements / collisions?

Next up: Factor out a proper atomic counter library
===================================================

Ok, I created two versions, corresponding to the previous raw CAS and
fake CAS strategies.  It fails as usual with raw CAS.  And it also
fails with the simulated CAS.  Here's an example with two too many
elements popped:

    Final sum: 6249998697052, producer/consumer/leftover sums: ([(1389411,1732239601843),(1382086,1726959654896)],[(1115131,1395897141782),(1113374,1394902298531)],[(0,0),(0,0)])
    Total pop events: 5000002 should be 5000000
    Checking that queue is finally null...
      :test_random_work_stealing: [Running]
      :test_random_work_stealing: [Failed]
    Correct final sum
    expected: 6249997500000
     but got: 6249998697052


[2013.06.26] {Everything seems good ... except}
-----------------------------------------------

I just got this exception.  BUT, that is surely due to sloppiness.  I
just did several installs with the older cabal, and while I *thought*
I disabled profiling avoiding the bug, I may have messed up.

    Grow to size 65536, copying over 32767
       [Setting # capabilities to 4 before test]
    Segmentation fault: 11

You can do a fresh build/test like this:

    ./install_all.sh --enable-tests

That will build normal and profiling, and exec normal.

Wait a sec... that passes... including chaselev:

    Test suite test-chaselev-deque: RUNNING...
    Test suite test-chaselev-deque: PASS

But then, immediately after...

    cd ChaseLev
    ./dist/build/test-chaselev-deque/test-chaselev-deque    
    ..........
    [Setting # capabilities to 4 before test]
    Segmentation fault: 11

Under valgrind I have this problem:

    N2_standalone_pushPop2: [Failed]
    ERROR: <stdout>: commitBuffer: invalid argument (Bad file descriptor)
    

Ah, wait.... to get cleaner build I need to actually wipe out prior
builds and force reinstall.  Ok, did that and still segfaulted even after running like this:

    export CABAL=cabal-1.17.0_HEAD    
    ./install_all.sh --enable-tests --disable-documentation --disable-library-profiling    
    cd ChaseLev
    ./dist/build/test-chaselev-deque/test-chaselev-deque    
    
But nevertheless it KEEPS passing when run by cabal --enable-tests!
(And I know it's really running based on CPU use.)  Just not when run
manually.  Further, the --with-ghc arg seems to be correct... it's
pointing it at ghc-7.6.2 on my laptop.

How about with profiling ON?
    
    ./install_all.sh --enable-tests --disable-documentation --enable-library-profiling --enable-executable-profiling    
    
Well that one doesn't segfault... But it does get to a point where it
seems to lock up even though it is using minimal CPU....  (erk, it
non-deterministically gets stuck like that; most time it
passes.... and *there* it segfaulted also).

I'm still thinking that this is the cabal bug rearing its head
*somehow*, or a problem with test-framework.

It is true that the recent refactoring to move the stdTestHarness code
into AbstractDeque has moved it from an executable context to a
library context, potentially changing its profiling status.chas

CONCLUSION: I think this is literally a terminal access problem as
suggested by the valgrind error above.  I can get both the hang and
the segfault (even without that latest refactoring), when I run from
iTerm, but I can't get either when I pipe to a file.  Of course, cabal
pipes to a file, so it doesn't get this problem.

AHa, to be more precise... the
"[Setting # capabilities to 2 before test]" messages are enough to
trigger this.  If I do NUMTHREADS=4 (and ditch those messages) then I
don't have a problem.

Aha... that's a printf, not a normal Haskell putStrLn... interesting.
But this is still weird because I'm telling test-framework "-j1".

In any case, I am disabling those messages.


[2013.07.17] {Do some performance measurements...}
===================================================




[2013.07.18] {Uh oh, what's this}
---------------------------------

Deja vu, I just started seeing this on hive (but not on limestone):

    time DEBUG=1 NUMELEMS=500000 NUMTHREADS=4 ./dist/build/test-chaselev-deque/test-chaselev-deque -j1 -t :ChaseLev_work-stealing-deque-tests_test_random_work_stealing

    Final sum: 62500481741, producer/consumer/leftover sums: ([(181612,22662208533),(140427,16071400039)],[(101389,13641612581),(76576,10125260588)],[(0,0),(0,0)])
    Total pop events: 500004 should be 500000
    Checking that queue is finally null...
    :ChaseLev_work-stealing-deque-tests_test_random_work_stealing: [Failed]

It happens constently at four threads and not at all at 2 or 3.

