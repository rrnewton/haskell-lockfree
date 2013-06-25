

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
    
    
