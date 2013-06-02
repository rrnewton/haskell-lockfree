

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

