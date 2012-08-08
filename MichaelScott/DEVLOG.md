
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

