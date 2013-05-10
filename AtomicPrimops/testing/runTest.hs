
import GHC.Conc
import System.Process
import System.Exit

-- Run the testing executable with different thread settings.
main = do
--  p <- getNumProcessors
  putStrLn "[TestHarness] Calling compiled test-atomic-primops executable..."
  ExitSuccess <- system "./dist/build/test-atomic-primops/test-atomic-primops +RTS -N1"
  -- TEMP: Fixing this at four processors because it takes a REALLY long time at larger numbers:
  ExitSuccess <- system$"./dist/build/test-atomic-primops/test-atomic-primops +RTS -N4"
  -- It does 248 test cases and takes 55s at -N16...
--  ExitSuccess <- system$"./dist/build/test-atomic-primops/test-atomic-primops +RTS -N"++show p
  return ()
