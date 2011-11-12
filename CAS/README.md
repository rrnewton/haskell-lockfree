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
  
  
  
  




