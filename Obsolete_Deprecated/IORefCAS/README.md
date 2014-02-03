See haddock in Data.CAS


A few notes on performance results
==================================


[2011.11.12] Initial Measurements
---------------------------------

  An initial round of tests gives the following results when executing
  100K CAS's from each of four threads on a 3.33GHz Nehalem desktop:

    RAW Haskell CAS:  0.143s        25% productivity 58MB alloc,  204,693 successes
    'Fake' CAS:       0.9s - 1.42s  89% productivity, 132M alloc, 104,044 successes
    Foreign CAS:      1.6s          23% productivity, 82M alloc,  264,821 successes

  "Successes" counts the total number of CAS operations that actually
  succeeded.

  This microbenchmark is spending a lot of its time in Gen 0 garbage collection.

  Next, a million CAS's per thread:

    RAW Haskell CAS:  0.65 - 1.0    20% productivity 406MB alloc (can stack overflow)
    'Fake' CAS:       14.3 - 17s    270% CPU  92% productivity 1.3GB alloc  1,008,468 successes
    Foreign CAS:      Stack overflow after 28.5s ... 300-390% CPU, 15% productivity, 

                      After bumping the stack size up it takes a long
                      time to finish, even after it has printed the
                      sample success bit vectors.
		      
		      With -K100M:
		      78s           6% productivity, 898M alloc,  3,324,943 successes
                                    67 seconds elapsed in Gen 0 GC!!

  Something odd is going on here.  How could it spend so long in GC
  for so little allocation??  For only two threads the Foreign
  implementation drops to 22s, but still 17.97s elapsed in Gen 0 GC.
  
  What about the Raw haskell CAS?  It also wil stack overflow with the
  current version of the test.  With a 1G stack it can do 5Mx4 CAS's
  (8,9M successful) in 6.7 seconds.  10M in 17s.  And STILL not seeing
  the previous segfault with the Raw CAS version...
  
[2011.11.12] Simple Stack Overflow Fix
--------------------------------------

  After making sure that all the (+1)'s in the test are strict, the
  stack overflow goes away and the numbers change (Raw does 5M in 3.3s
  instead of 6.7s).  BUT there's still quite a lot of time spent in
  GC.  Here's 1Mx4 CAS's again:
      
    RAW Haskell CAS:  0.7s  (23% prod, 0.8s total Gen 0 GC)
    'Fake' CAS:       11.8s (91% prod, 0.8s total Gen 0 GC)
    Foreign CAS:      52s  (6% prod)
  
  And then adding -A1M makes a neglible change in runtime for Raw, but
  reduces the # of gen0 collections from 484 to 235.

  Ok, how about testing on a 3.1GHz Westmere.
  Wow, just ran into this:

    cc1: internal compiler error: Segmentation fault
    Please submit a full bug report,
    with preprocessed source if appropriate.
    See <http://bugzilla.redhat.com/bugzilla> for instructions.
    make: *** [all] Error 1

  On a different machine it worked (default runtime flags 1Mx4 CAS):

    RAW Haskell CAS:  0.7s  
    'Fake' CAS:       8.1s
    Foreign CAS:      46s

  The lack of hyperthreading may also be helping.

 <TODO>: 
  The primary source of allocation in this example is the accumulation
  of the [Bool] lists of success and failure.  I should disable those
  and test again.


[2011.11.13] Testing specialized CAS.Foreign instance
-----------------------------------------------------

All of the above results were for a cell containing an Int.  That
would not have triggered the specialized (Storable-based) instance in
Foreign.hs.  There SHOULD be special cases for all word-sized scalars,
but currently there's just one for Word32.  Let's test that one.

    Word32 1Mx4 CAS's:
    RAW Haskell CAS:  0.57s  (13.7% prod)
    Foreign CAS:      0.64s  (15% prod)

Wow, the foreign one is doing as well as the Haskell one even though
there's some extra silliness in the Foreign.CASRef type (causing a
runtime case dispatch to unpack).


[2011.11.13] Testing atomicModify based on CAS
----------------------------------------------

An atomicModify based on CAS offers a drop-in replacement that could
improve performance.  I implemented one which will try CAS until it
fails a certain number of times ("30" for now, but needs to be tuned).

These seem to work well.  They are cheaper than you would think given
how long it takes to get successful CAS attempts under contention:

 1Mx4 CAS attempts or atomicModifies:
    0.19s  -- CAS attempts 1.07M successful.
    0.02s  -- 1M   atomicModifyIORefCAS on 1 thread
    0.13s  -- 1Mx2 atomicModifyIORefCAS on 2 threads
    0.37s  -- 1Mx4 atomicModifyIORefCAS on 4 threads

And they are cheaper than the real atomicModifyIORef (which also seems
to have a stack space problem right now because of its laziness).  But
with a huge stack (1G) it will succeed:

    1.27s  -- 1Mx4 atomicModifyIORef

But inserting extra strictness (an evaluate call) to avoid the
stack-leak actually makes it slower:

    2.08s  -- 1Mx4 atomicModifyIORef, stricter

