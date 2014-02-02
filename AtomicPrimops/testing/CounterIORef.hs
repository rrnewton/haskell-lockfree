{-# LANGUAGE CPP, BangPatterns #-}
module CounterIORef (tests) where
import qualified Data.Atomics.Counter.IORef as C

#include "CounterCommon.hs"

name = "IORef"

default_seq_tries  = 10 * numElems
-- Things are MUCH slower with contention:
default_conc_tries = numElems
