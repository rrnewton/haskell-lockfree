
import GHC.Conc
import System.Process
import System.Exit

-- Run the testing executable with different thread settings.
main = do
--  p <- getNumProcessors
  putStrLn "[TestHarness] Calling compiled test-atomic-primops executable..."
  ExitSuccess <- system "./dist/build/test-atomic-primops/test-atomic-primops +RTS -N1"
  ExitSuccess <- system$"./dist/build/test-atomic-primops/test-atomic-primops +RTS -N"
--  ExitSuccess <- system$"./dist/build/test-atomic-primops/test-atomic-primops +RTS -N"++show p
  return ()
