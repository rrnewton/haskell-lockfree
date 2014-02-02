{-# LANGUAGE CPP, BangPatterns #-}
module CounterForeign (tests) where
import qualified Data.Atomics.Counter.Foreign as C

#include "CounterCommon.hs"

name = "Foreign"

-- This version is much slower than some of the others:
default_seq_tries  = 10 * base 
default_conc_tries = base

base = numElems `quot` 15

