
[2011.12.09] Some of the strange errors one can encounter presently
-------------------------------------------------------------------

Here are two consecutive runs of Test.hs in this directory:

    Reading sums from MVar...
    Test: internal error: PAP object entered!
	(GHC version 7.2.1 for x86_64_unknown_linux)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    Aborted

And then:

    Test: internal error: evacuate: strange closure type -958224799
	(GHC version 7.2.1 for x86_64_unknown_linux)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

On Mac OS it generates the same "evacuate" error (or just segfaults).

Note, as mentioned in the comments this is because of using native CAS
rather than the Fake version.


[2011.12.09] Observing large number of retries with Fake CAS -O2
----------------------------------------------------------------

Most of the time it NEVER trips the warning at 1000 tries in spinPop. 

But then I'll see things like this:

   Maximum retries for each consumer thread: [181000]

Jeez!  And that's with ONE consumer and ONE producer thread!!  How can
the producer lock out the consumer for that long?


[2011.12.09] Observing non-termination with BOTH CAS -O2
----------------------------------------------------------

When running on larger number of threads the program can sometimes
spin forever!  I.e. items must be being lost and then the consumers
spin forever!

This happens both with native and with fake CAS.  It's therefore
probably a bug in the queue implementation or the benchmark.

It happens even with a small number of items (1000).

It also happens with -N2 instead of -N4.  And it happens on -O0.


[2011.12.29] Stack overflows during testing
-------------------------------------------

This was the same problem that I observed when testing the reference
implementation.

   Divergence problem:
   -------------------

Actually, in additon to this problem I'll ALSO seeing some
nontermination.  It looks like there may be a bug that's losing data
leaving consumers to wait forever...  Under -N4 it when it diverges it
uses 200% CPU which is consistent with this hypothesis (i.e. the
producers complete but the consumers are spinning).

Hmm.. But when I try to put a cap on the amount a consumer will spin
(terminating in an error after a while), I never reach that cap after
running over a minute!  That's weird.  (And this remains true even if
I make that cap "10"... weird.  Further I included a print "." in
spinPop and thereby can see that none of them are spinning...)

The process isn't using much memory, and it stays constant... How can
it be burning cycles if it is NOT in spinPop?  

This divergence problem happens under BOTH ghc-7.2.1 and 7.4 RC1 and
with both Native and Fake CAS.

   Another Hypethesis
   ------------------

We MIGHT hypethesize that this is a problem with spinPop not
allocating and therefore not being preempted by the GHC scheduler.
But that would be a problem if we had MORE Haskell threads than OS
threads.  In this case we have the same number.....

Although... there's no guarantee about thread load balancing... we're
not using forkOn, so that COULD be an issue in this benchmark.  And
yet, we know we're not spinning in spinPop because we're not seeing
the printout.  Where are we spinning?


[2011.12.29] Continued Divergence Debugging
===========================================

 "-N2 vs -N4"

Note that under -N2 it DOES hit the 1M cap on spinPop EVEN with the
recently added "yield" in spinPop.


[2012.08.08] Back to debugging with help from Andreas.

I just confirmed that on hive with -N32 I definitely see a deadlock.

Go Andreas!  There was flagrantly incorrect code at the end of pushL
which he caught.  That one fix corrects the deadlock for me when
running -N32.

[2012.08.10] {Differing performance across small and large SMPs}

I will be referring to these machines:

 * hive -- 32 core westmere @ 2.13 ghz
 * basalt -- 4 core westmere @ 3.1 ghz
 * veronica -- 2 core harpertown @ 3ghz

Here are the times for running:

    time ./Test.exe -t Half +RTS -N2 

 * hive     -- 400-700ms
 * basalt   -- ~200ms
 * veronica -- ~300ms

Why does it do so badly on the bigger SMP EVEN whene there are only 2
threads.  Remember, in the current version of this benchmark, we are
using forkOS -- the threads are real OS threads.  Is the problem that
they're getting placed on different chips?

And then what about adding more parallelism & contention?  But recall
that this benchmark is pushing exactly 500K elements through the queue
IRRESPECTIVE of how many threads are used.  So it is doing the same
amount of "work" in each case, just under more contention.

    time ./Test.exe -t Half +RTS -N4

 * hive     -- ~1000 ms
 * basalt   -- ~100  ms
 * veronica -- ~1000 ms 

    time ./Test.exe -t Half +RTS -N8

 * hive     -- 1 - 1.5s
 * basalt   -- 1.2 - 1.5s 
 * veronica -- 2.3 - 2.5s

    time ./Test.exe -t Half +RTS -N16

 * hive     -- 1.3 - 2.0s
 * basalt   -- 1.6 - 2.3
 * veronica -- 4.4s - 6s


    time ./Test.exe -t Half +RTS -N32

 * hive     -- 2.2 - 3.0s
 * basalt   -- 3.3 - 3.8s 
 * veronica -- 8.5s

Are there issues here with blackholing and OS level context
switching??  We really need to test this while changing forkOS/forkIO
as well as -N.

I am seeing NO retries on the consumer threads in ANY of the above...
This would mean that the backoff that's built into the benchmark
itself (spinPop function calls yield after failure and threadDelay
after more than 1000 -- VERY aggressive backoff).  OH WAIT the current
version of tryPop always retries under contention and only fails if
the queue is empty.  So we would not expect contention to result in
retries at the level of "spinPop".








