
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

