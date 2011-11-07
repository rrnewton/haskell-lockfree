
import Data.IORef
-- import Data.CAS
-- import Data.CAS.Fake
import Data.CAS.Foreign

testCAS = 
  do let zer = (0::Int)
--     r <- newIORef zer
     r <- mkCASable zer
     let loop 0 = return ()
	 loop n = do

          (b,v) <- cas r zer 100  -- Must use "zer" here.

--          (b,v) <- casIORef r zer 100  -- Must use "zer" here.
--          (b,v) <- casStrict r 0 100  -- Otherwise this is nondeterministic based on compiler opts.
		   -- Sometimes the latter version works on the SECOND evaluation of testCAS.  Interesting.
          putStrLn$ "After CAS " ++ show (b,v)
          loop (n-1)
     loop 10 

     x <- readCASable r
     putStrLn$ "Finished with loop, read cell: " ++ show x
     writeCASable r 111
     y <- readCASable r
     putStrLn$ "Wrote and read again read: " ++ show y
     return ()


main = testCAS



-- test x = do
--   a <- newStablePtr x 
--   b <- newStablePtr x 
--   printf "First call, word %d IntPtr %d\n" 
-- 	 (unsafeCoerce a :: Word)
-- 	 ((fromIntegral$ ptrToIntPtr $ castStablePtrToPtr a) :: Int)
--   printf "Second call, word %d IntPtr %d\n" 
-- 	 (unsafeCoerce b :: Word)
-- 	 ((fromIntegral$ ptrToIntPtr $ castStablePtrToPtr b) :: Int)


-- main = test 3
