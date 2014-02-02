{-# LANGUAGE CPP, BangPatterns #-}
module CounterUnboxed (tests) where
import qualified Data.Atomics.Counter.Unboxed as C

#include "CounterCommon.hs"

name = "Unboxed"

default_seq_tries  = 10 * numElems
-- Things are MUCH slower with contention:
default_conc_tries = numElems
