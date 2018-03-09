{-# LANGUAGE CPP #-}

import Data.Atomics
import Test (test_all_hammer_one)
import Criterion.Types (Benchmarkable, toBenchmarkable)
import Criterion.Main
import Control.DeepSeq
import Control.Exception (evaluate)
import Data.IORef
import qualified Data.Atomics as A
import qualified Data.Atomics.Counter as C

#if !(MIN_VERSION_deepseq(1,4,2))
instance NFData (IORef a) where rnf _ = ()
#endif
instance NFData C.AtomicCounter where rnf _ = ()

b0 :: Benchmarkable
b0 = toBenchmarkable $ \iters -> test_all_hammer_one 4 (fromIntegral iters) 0

b1 :: Benchmark
b1 = env (newIORef (0::Int)) $ \ ref ->
   bench "CAS_incr" $ nfIO $ do
     t <- readForCAS ref
     _ <- casIORef ref t (peekTicket t + 1)
     return ()

b2 :: Benchmark
b2 = env (newIORef (0::Int)) $ \ ref ->
   bench "seq_incr" $ nfIO $ do
     x <- readIORef ref
     y <- evaluate (x+1)
     _ <- writeIORef ref y
     return ()

b3 :: Benchmark
b3 = env (C.newCounter (0::Int)) $ \ ref ->
   bench "atomic_counter" $ nfIO $ do
     C.incrCounter 1 ref

main :: IO ()
main = defaultMain
       [ b1, b2, b3
         -- bench "test_all_hammer_one" b0
       ]
