
-- A simple test that verifies the compile & link went ok for atomic-primops.

import Data.IORef
import Data.Atomics

main = do 
  putStrLn "hello"
  x <- newIORef (3::Int)
  t <- readForCAS x
  casIORef x t 4
  print =<< readIORef x 

