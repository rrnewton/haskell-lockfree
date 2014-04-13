
import Control.Monad
import Data.IORef
import Data.Atomics
import Data.Atomics.Internal (ptrEq)

main = do
  r <- newIORef "hi"
  t0 <- readForCAS r
  (True,t1) <- casIORef r t0 "bye"
  -- putStrLn$ "First CAS succeeded? "++show b1
  -- putStrLn$ "Tickets pointer equal? " ++ show (t0 == t1)
  -- (b2,t2) <- casIORef r t1 "bye2"
  -- putStrLn$ "Second CAS succeeded? " ++ show b2
  -- unless (b1 == True) $ error "Test failed"
  return ()


