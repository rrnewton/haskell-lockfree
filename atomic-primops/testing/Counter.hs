{-# LANGUAGE CPP #-}
module Counter (tests) where
import qualified Data.Atomics.Counter as C

#include "CounterCommon.hs"

name :: String
name = "Unboxed"

default_seq_tries, default_conc_tries :: Int
default_seq_tries  = 10 * numElems
-- Things are MUCH slower with contention:
default_conc_tries = numElems

