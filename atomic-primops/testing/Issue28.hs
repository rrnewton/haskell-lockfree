
module Issue28 (main) where

-- import Control.Monad
import Data.IORef
import Data.Atomics
-- import Data.Atomics.Internal (ptrEq)

main :: IO ()
main = do
  putStrLn "Issue28: Conducting the simplest possible read-then-CAS test."
  r <- newIORef "hi"
  t0 <- readForCAS r
  (True,t1) <- casIORef r t0 "bye"
  -- putStrLn$ "First CAS succeeded? "++show b1
  -- putStrLn$ "Tickets pointer equal? " ++ show (t0 == t1)
  -- (b2,t2) <- casIORef r t1 "bye2"
  -- putStrLn$ "Second CAS succeeded? " ++ show b2
  -- unless (b1 == True) $ error "Test failed"  

  putStrLn$  "Issue28: test passed "++show t1
