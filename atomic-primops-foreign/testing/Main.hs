
module Main where

import GHC.Conc
import Test.Framework  (Test, defaultMain, testGroup)
import qualified CounterForeign
import Control.Monad (when)

----------------------------------------

main :: IO ()
main = do
       -- TEMP: Fixing this at four processors because it takes a REALLY long time at larger numbers:
       -- It does 248 test cases and takes 55s at -N16...
       -- numcap <- getNumProcessors
       let numcap = 4
       when (numCapabilities /= numcap) $ setNumCapabilities numcap
       defaultMain CounterForeign.tests
