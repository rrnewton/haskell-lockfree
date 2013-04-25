{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

module Main where
import Data.Concurrent.Deque.Tests        (test_fifo_OneBottleneck, numElems)
import Data.Concurrent.Queue.MichaelScott (newQ, LinkedQueue)

{-# SPECIALIZE test_fifo_OneBottleneck :: Bool -> Int -> LinkedQueue Int -> IO () #-}

-- Benchmark mode:
main :: IO ()
main = newQ >>= test_fifo_OneBottleneck True numElems 
