{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP, BangPatterns, OverlappingInstances 
    , FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances
  #-}

-- | This test has three different modes which can be toggled via the
-- C preprocessor.  Any subset of the three may be activated.

import Control.Monad
import Control.Exception
import Control.Concurrent.MVar
import GHC.Conc
import Data.IORef
import Data.Word
import Data.CAS.Internal.Class
import Data.Time.Clock
import System.Environment
import System.Mem.StableName
import GHC.IO (unsafePerformIO)

#ifdef T1
import qualified Data.CAS.Internal.Native  as A
#endif
#ifdef T2
import qualified Data.CAS.Internal.Fake    as B
#endif
#ifdef T3
import qualified Data.CAS.Internal.Foreign as C
#endif

import Text.Printf

----------------------------------------------------------------------------------------------------
-- TODO:
--  * Switch the [Bool] implementation to use a BitList.
--  
----------------------------------------------------------------------------------------------------

{-# NOINLINE zer #-}
zer = 0

-- iters = 100
-- iters = 10000
default_iters = 100000
-- iters = 1000000

----------------------------------------------------------------------------------------------------
-- Helpers

printBits = print . map pb
 where pb True  = '1' 
       pb False = '0'

forkJoin :: Int -> (IO b) -> IO [b]
forkJoin numthreads action = 
  do
     answers <- sequence (replicate numthreads newEmptyMVar) -- padding?
     printf "Forking %d threads.\n" numthreads
    
     forM_ answers $ \ mv -> 
 	forkIO (action >>= putMVar mv)

     -- Reading answers:
     ls <- mapM readMVar answers
     printf "All %d thread(s) completed\n" numthreads
     return ls

-- Describe a structure of forking and joining threads for tests:
data Forkable a = Fork Int (IO a)
                | Parallel (Forkable a) (Forkable a) -- Parallel composition
                | Sequence (Forkable a) (Forkable a) -- Sequential compositon, with barrier
--                | Barrier Forkable


timeit ioact = do 
   start <- getCurrentTime
   res <- ioact
   end   <- getCurrentTime
   putStrLn$ "  Time elapsed: " ++ show (diffUTCTime end start)
   return res

----------------------------------------------------------------------------------------------------

-- The element type for our CAS test.
-- type ElemTy = Int
type ElemTy = Word32 -- This will trigger CAS.Foreign's specialization.

{-# INLINE testCAS1 #-}
-- First test: Run a simple CAS a small number of times.
testCAS1 :: CASable ref ElemTy => ref ElemTy -> IO [Bool]
testCAS1 r = 
  do 
     bitls <- newIORef []
--     let zer = (0::Int)

--     r :: CASRef Int <- newCASable zer 
     let loop 0 = return ()
	 loop n = do

          (b,v) <- cas r zer 100  -- Must use "zer" here.
          atomicModifyIORef bitls (\x -> (b:x, ()))

--          (b,v) <- casIORef r zer 100  -- Must use "zer" here.
--          (b,v) <- casStrict r 0 100  -- Otherwise this is nondeterministic based on compiler opts.
		   -- Sometimes the latter version works on the SECOND evaluation of testCAS.  Interesting.
          putStrLn$ "  After CAS " ++ show (b,v)
          loop (n-1)
     loop 10

     x <- readCASable r
     putStrLn$ "  Finished with loop, read cell: " ++ show x
     writeCASable r 111
     y <- readCASable r
     putStrLn$ "  Wrote and read again read: " ++ show y

     ls <- readIORef bitls
     return (reverse ls)


----------------------------------------------------------------------------------------------------

-- This version hammers on CASref from all threads, then checks to see
-- if enough threads succeeded enough of the time.

-- If each thread tries K attempts, there should be at least K
-- successes.  To establish this consider the inductive argument.  One
-- thread should succeed all the time.  Adding a second thread can
-- only foil the K attempts of the first thread by itself succeeding
-- (leaving the total at or above K).  Likewise for the third thread
-- and so on.

-- Conversely, for N threads each aiming to complete K operations,
-- there should be at most N*N*K total operations required.

testCAS2 :: CASable ref ElemTy => Int -> ref ElemTy -> IO [[Bool]]
testCAS2 iters ref = 
  forkJoin numCapabilities $ 
    do 
       let loop 0 expected !acc = return (reverse acc)
	   loop n expected !acc = do
            -- let bumped = expected+1 -- Must do this only once, should be NOINLINE
	    bumped <- evaluate$ expected+1 
	    (b,v) <- cas ref expected bumped
            when (iters < 30) $ 
              putStrLn$ "  Attempted to CAS "++show bumped ++" for "++ show expected ++ " (#"++show (unsafeName expected)++"): " 
			++ show b ++ " found " ++ show v ++ " (#"++show (unsafeName v)++")"
	    if b 
             then loop (n-1) bumped (b:acc)
             else loop (n-1) v      (b:acc)

       init <- readCASable ref
       loop iters init []


--------------------------------------------------------------------------------

-- UNFINISHED
-- This tests repeated atomicModifyIORefCAS operations.

testCAS3 :: Int -> IORef ElemTy -> IO [()]
testCAS3 iters ref = 
  forkJoin numCapabilities (loop iters)
 where 
   loop 0  = return ()
   loop n  = do
	-- let bumped = expected+1 -- Must do this only once, should be NOINLINE
--        let bump !x !y = x+y
#ifdef T1
	A.atomicModifyIORefCAS_ ref (+1)
#endif
#ifdef T2
--	B.atomicModifyIORefCAS_ ref (+1)
--	B.atomicModifyIORefCAS_ ref (bump 1)
	x <- atomicModifyIORef ref (\x -> (x+1,x))
        evaluate x -- Avoid stack leak.
#endif
	loop (n-1)

----------------------------------------------------------------------------------------------------       
-- This version uses a non-scalar type for CAS.  It instead
-- manipulates the tail pointers of a simple linked-list.

#if 0
data List k = Null | Cons Int (k (List k))

type ListA = List A.CASRef
type ListB = List B.CASRef
type ListC = List C.CASRef

-- testCAS4 :: CASable ref Int => List ref -> IO [Bool]
testCAS4 :: CASable ref Int => Int -> ref (List ref) -> IO ()
testCAS4 iters ref = do 
  forkJoin numCapabilities $ do
     -- From each thread, attempt to extend the list 'iters' times:
     ref' <- readCASable ref
     nl   <- newIORef Null
     loop iters (Cons (-1) nl) ref'
     return ()

  return ()
 where 
  loop 0 _ _ = return ()
  loop n new (Cons _ tl) = do
    tl' <- readCASable tl
    case tl' of 
      Null -> do (b,v) <- cas tl tl' new
		 if b then loop (n-1) v
		      else loop v
      cons -> loop cons tl'
  loop n _ Null = error "too short"
#endif


----------------------------------------------------------------------------------------------------
-- Test Oracles

checkOutput1 msg ls =
  if ls == True : replicate (9) False
  then return ()
  else error$ "Test "++ msg ++ " failed to have the right CAS success pattern: " ++ show ls

checkOutput2 :: String -> Int -> [[Bool]] -> ElemTy -> IO ()
checkOutput2 msg iters ls fin = do 
  let totalAttempts = sum $ map length ls
  putStrLn$ "Final value "++show fin++", Total successes "++ show (length $ filter id $ concat ls)
  when (fin < fromIntegral iters) $
    error$ "ERROR in "++ show msg ++ " expected at least "++show iters++" successful CAS's.." 

checkOutput3 :: String -> Int -> [[Bool]] -> ElemTy -> IO ()
checkOutput3 msg iters ls fin = do 

  return ()


----------------------------------------------------------------------------------------------------

main = do 
   args <- getArgs
   let iters = 
        case args of 
	 []  -> default_iters
	 [a] -> read a
	 ls  -> error$ "Wrong number of arguments to executable: " ++ show ls
 
#ifdef T1
   putStrLn$ "\nTesting Raw, native CAS:"
   o1A <- (newCASable zer :: IO (A.CASRef ElemTy)) >>= testCAS1
   checkOutput1 "Raw 1"     o1A
#endif
#ifdef T2
   putStrLn$ "\nTesting Fake CAS, based on atomicModifyIORef:"
   o1B <- (newCASable zer :: IO (B.CASRef ElemTy)) >>= testCAS1
   checkOutput1 "Fake 1"    o1B
#endif
#ifdef T3
   putStrLn$ "\nTesting Foreign CAS, using mutable cells outside of the Haskell heap:"
--   o1C <- (newCASable zer :: IO (C.CASRef ElemTy)) >>= testCAS1
   o1C <- (newCASable zer :: IO (C.CASRef ElemTy)) >>= testCAS1
   checkOutput1 "Foreign 1" o1C
#endif

   ------------------------------------------------------------

#ifdef T1
   putStrLn$ "\nTesting Raw, native CAS:"
   ref   <- newCASable zer :: IO (A.CASRef ElemTy)
   o2A   <- timeit$ testCAS2 iters ref
   mapM_ (printBits . take 100) o2A
   fin2A <- readCASable ref
   checkOutput2 "Raw 1"     iters o2A fin2A
#endif
#ifdef T2
   putStrLn$ "\nTesting Fake CAS, based on atomicModifyIORef:"
   ref   <- newCASable zer :: IO (B.CASRef ElemTy)
   o2B   <- timeit$ testCAS2 iters ref
   mapM_ (printBits . take 100) o2B
   fin2B <- readCASable ref
   checkOutput2 "Fake 1"    iters o2B fin2B
#endif
#ifdef T3
   putStrLn$ "\nTesting Foreign CAS, using mutable cells outside of the Haskell heap:"
   ref   <- newCASable zer :: IO (C.CASRef ElemTy)
   o2C   <- timeit$ testCAS2 iters ref
   mapM_ (printBits . take 100) o2C
   fin2C <- readCASable ref
   checkOutput2 "Foreign 1" iters o2C fin2C
#endif

   ------------------------------------------------------------

#ifdef T1
   putStrLn$ "\nTesting atomicModifyIORefCAS, native CAS:"
--   ref   <- newCASable zer :: IO (A.CASRef ElemTy)
   ref   <- newIORef zer :: IO (IORef ElemTy)
   o3A   <- timeit$ testCAS3 iters ref
--   mapM_ (printBits . take 100) o3A
--   fin3A <- readCASable ref
   fin3A <- readIORef ref
--   checkOutput3 "Raw 2"     iters o3A fin3A
   putStrLn$ "  Final sum: "++ show fin3A
#endif
#ifdef T2
   putStrLn$ "\nTesting atomicModifyIORefCAS:"
--   ref   <- newCASable zer :: IO (B.CASRef ElemTy)
   ref   <- newIORef zer :: IO (IORef ElemTy)
   o3B   <- timeit$ testCAS3 iters ref
--   mapM_ (printBits . take 100) o3B
--   fin3B <- readCASable ref
   fin3B <- readIORef ref
--   checkOutput3 "Fake 2"    iters o3B fin3B
   putStrLn$ "  Final sum: "++ show fin3B
#endif
-- #ifdef T3
--    putStrLn$ "\nTesting Foreign CAS, using mutable cells outside of the Haskell heap:"
--    ref   <- newCASable zer :: IO (C.CASRef ElemTy)
--    o3C   <- testCAS3 iters ref
--    mapM_ (printBits . take 100) o3C
--    fin3C <- readCASable ref
--    checkOutput3 "Foreign 1" iters o3C fin3C
-- #endif

   ------------------------------------------------------------

   putStrLn$ "\nAll test outputs looked good."


{-
  [2011.11.10] 

Well... just got this output from the WRONG test:

"1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"
"1010101010101010101010101010101010101010101010101010101010101010101010010010100100101010101001001001"
CURRENTLY THIS SHOULD NEVER HAPPEN BECAUSE THE FINALIZER KEEPS IT ALIVE!
"0101010101010100010101010101010101010101010101010101010101010101010101010101010101010101010101010101"
"0100010010001001001001001010101010101001010101010101010101010101010101010101010101001010101010101010"

The first problem is that this indicates a bug, the second is that it's coming from the WRONG PLACE.

Let me be more specific.  I'm testing three versions.  On Mac OS I see
the failure in the Foreign.hs, which is where the error message is
located and where it's coming from!

     Testing Raw, native CAS:
     Forking 2 threads.
     All threads 2 completed
     "1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"
     "1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"

     Testing Fake CAS, based on atomicModifyIORef:
     Forking 2 threads.
     All threads 2 completed
     "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
     "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

     Testing Foreign CAS, using mutable cells outside of the Haskell heap:
     Forking 2 threads.
     All threads 2 completed
     CURRENTLY THIS SHOULD NEVER HAPPEN BECAUSE THE FINALIZER KEEPS IT ALIVE!
     "1010101010101010101010101010101010101010101010101010101010101010101010101001001001001001001001001001"
     "0100100100100100100100100100101010101010101010101010101010101010101010101010101010101010101010101010"

But on Linux I see this error coming from the test for A.CASRef!

    Testing Raw, native CAS:
    Forking 4 threads.
    All threads 4 completed
    "1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"
    "1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"
    CURRENTLY THIS SHOULD NEVER HAPPEN BECAUSE THE FINALIZER KEEPS IT ALIVE!
    "1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"
    "1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010"

    Testing Fake CAS, based on atomicModifyIORef:
    Forking 4 threads.
    All threads 4 completed
    "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

    Testing Foreign CAS, using mutable cells outside of the Haskell heap:
    Forking 4 threads.
    All threads 4 completed
    "0010010100101000101010101000101010100101010010101010010100101010100000100100100010101010100101001010"
    "1010101010101010100001000101001010010101000100101010000001000010000100010000001000000001000001010001"
    "0010010010000010000000010000000000010000100001000100101001010001000100100010010010101001001010001001"
    "0010101010100100101001000100101000001000100001000100100010001001010101010101010101010101010101010101"
    Final value 200, Total successes 200
    Final value 2, Total successes 2
    test.exe: ERROR in "Fake 1" expected at least 100 successful CAS's..

As though the instances are getting mixed up or selected in a nondeterministic way.

A.CASRef B.CASRef and C.CASRef should be unique types which do not unify with one another....

If I pump up the numbers I start seeing segfaults, which appear to be
coming from the foreign version but I think that's just because they get swapped!...

OR it's possible that I'm being silly and that I have not put
sufficient barriers between the phases to FORCE all work to complete
and therefore all print messages to be printed out in order.

    Testing Foreign CAS, using mutable cells outside of the Haskell heap:
    Forking 4 threads.
    Segmentation fault

I'm ALSO seeing failures of insufficient successes on my laptop at iters=10K...

------------------------------------------------------------

Ok, going to attempt to tease this out by first testing only one implementation at a time:

  Data.CAS -- insufficient successes occasionally (10K), 
              insufficient successes always (100K),
	      stack overflow for this test (1M)


 -}



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
