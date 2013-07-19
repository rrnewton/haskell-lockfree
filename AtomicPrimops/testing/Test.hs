{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns, ScopedTypeVariables, NamedFieldPuns, CPP #-}

-- | This test has three different modes which can be toggled via the
-- C preprocessor.  Any subset of the three may be activated.

import Control.Monad
-- import Control.Monad.ST (stToIO)
import Control.Exception (evaluate)
import Control.Concurrent.MVar
import GHC.Conc
import Data.IORef (modifyIORef')
import Data.Int
import Data.Time.Clock
import Text.Printf
import GHC.STRef
import GHC.IORef
import GHC.Stats (getGCStats, GCStats(..))
import Data.Primitive.Array
import Data.Word
import qualified Data.Set as S
import System.Random (randomIO, randomRIO)

import Data.Atomics as A
import Data.Atomics (casArrayElem, readArrayElem)

import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework  (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import GHC.IO (unsafePerformIO)
import System.Mem (performGC)
import System.Mem.StableName (makeStableName, hashStableName)
import System.Environment (getEnvironment)
import System.IO        (stdout, stderr, hPutStrLn, hFlush)
import Debug.Trace      (trace)

import CommonTesting 
import CounterTests (counterTests)

------------------------------------------------------------------------

expect_false_positive_on_GC :: Bool
expect_false_positive_on_GC = False

getGCCount :: IO Int64 
getGCCount | expect_false_positive_on_GC = 
               do GCStats{numGcs} <- getGCStats
                  return numGcs
           | otherwise = return 0

main :: IO ()
main = do
       -- TEMP: Fixing this at four processors because it takes a REALLY long time at larger numbers:
       -- It does 248 test cases and takes 55s at -N16...
       -- numcap <- getNumProcessors
       let numcap = 4
       setNumCapabilities numcap
       
       defaultMain $ 
         [ testCase "casTicket1"              case_casTicket1
         , testCase "create_and_read"         case_create_and_read
         , testCase "create_and_mutate"       case_create_and_mutate
         , testCase "create_and_mutate_twice" case_create_and_mutate_twice
         , testCase "n_threads_mutate"        case_n_threads_mutate
         , testCase "run_barriers"            case_run_barriers

         , testCase "test_succeed_once Int"   (test_succeed_once (0::Int))
         , testCase "test_succeed_once Int64" (test_succeed_once (0::Int64))
         , testCase "test_succeed_once Word32" (test_succeed_once (0::Word32))
         , testCase "test_succeed_once Word16" (test_succeed_once (0::Word16))
         , testCase "test_succeed_once Word8"  (test_succeed_once (0::Word8))
         ]
         ++
  --       all_hammerConfigs (0::Int64)
         -- Test several configurations of this one:
         [ testCase ("test_all_hammer_one_"++show threads++"_"++show iters ++":")
                    (test_all_hammer_one threads iters (0::Int))
         | threads <- [1 .. 2*numcap]
         , iters   <- [1, 10, 100, 1000, 10000, 100000, 500000]] ++
         [ testCase ("test_hammer_many_threads_1000_10000:")
                    (test_all_hammer_one 1000 10000 (0::Int)) ]  ++

         [ testCase "casmutarray1"            case_casmutarray1] ++
         [ testCase ("test_random_array_comm_"++show threads++"_"++show size++"_"++show iters ++":")
                    (test_random_array_comm threads size iters)
         | threads <- filter (>0) $ setify $
                      [1, numcap `quot` 2, numcap, 2*numcap]
         , size    <- [1, 10, 100]
         , iters   <- [10000]]
         ++ counterTests

setify :: [Int] -> [Int]
setify = S.toList . S.fromList

------------------------------------------------------------------------
{-# NOINLINE mynum #-}
mynum :: Int
mynum = 33

-- Expected output: 
{---------------------------------------
    Perform a CAS within a MutableArray#
      1st try should succeed: (True,33)
    2nd should fail: (False,44)
    Printing array:
      33  33  33  44  33
    Done.
-}
case_casmutarray1 :: IO ()
case_casmutarray1 = do 
 putStrLn "Perform a CAS within a MutableArray#"
 arr <- newArray 5 mynum

 writeArray arr 4 33
 putStrLn "Wrote array elements..."
 
 tick <- readArrayElem arr 4
 putStrLn$ "(Peeking at array gave: "++show (peekTicket tick)++")"

 (res1,tick2) <- casArrayElem arr 3 tick 44
 (res2,_)     <- casArrayElem arr 3 tick 44
-- res  <- stToIO$ casArrayST arr 3 mynum 44
-- res2 <- stToIO$ casArrayST arr 3 mynum 44 

 putStrLn "Printing array:"
 forM_ [0..4] $ \ i -> do
   x <- readArray arr i 
   putStr ("  "++show x)

 assertBool "1st try should succeed: " res1
 assertBool "2nd should fail: " (not res2)


-- case_casbytearray1 :: IO ()
-- case_casbytearray1 = do 
--  putStrLn "Perform a CAS within a MutableByteArray#"

-- | This test uses a number of producer and consumer threads which push and pop
-- elements from random positions in an array.
test_random_array_comm :: Int -> Int -> Int -> IO ()
test_random_array_comm threads size iters = do 
  arr <- newArray size Nothing
  tick0 <- readArrayElem arr 0
  for_ 1 size $ \ i -> do
    t2 <- readArrayElem arr i
    assertEqual "All initial Nothings in the array should be ticket-equal:" tick0 t2

  ls <- forkJoin threads $ \tid -> do 
    localAcc <- newIORef 0
    for_ 0 iters $ \iter -> do
      -- Randomly pick a position:
      ix <- randomRIO (0,size-1) :: IO Int
      -- Randomly either produce or consume:
      b <- randomIO :: IO Bool
      if b then do 
        (b,newtick) <- casArrayElem arr ix tick0 (Just iter)
        return ()
       else do -- Consume:
        tick <- readArrayElem arr ix
        case peekTicket tick of
          Just _  -> do (b,x) <- casArrayElem arr ix tick (peekTicket tick0) -- Set back to Nothing.
                        when b $ modifyIORef' localAcc (+1)
--                        print (peekTicket x)
          Nothing -> return ()
        return ()
    readIORef localAcc
    
  let successes = sum ls
      -- Pidgeonhole principle.
      -- min_success =
  printf "Communication through random array positions (threads/size/iters %s).\n" (show (threads,size,iters))
  printf "Successes: %d (expected 1/4 of total iterations on all threads)\n" successes
  printf "Per-thread successes: %s\n" (show ls)
  assertBool "Number of successes: " (successes <= (threads * iters) `quot` 2 && successes >= 0)
  for_ 0 size $ \ i -> do
    x <- readArray arr i
--    putStr (show x ++ " ")
    return ()
  putStrLn ""
  return ()
  
   
----------------------------------------------------------------------------------------------------
-- Simple, non-parameterized tests
 ----------------------------------------------------------------------------------------------------

{-# NOINLINE zer #-}
zer :: Int
zer = 0
default_iters :: Int
default_iters = 100000

case_casTicket1 :: IO ()
case_casTicket1 = do
  dbgPrint 1 "\nUsing new 'ticket' based compare and swap:"

  IORef (STRef mutvar) <- newIORef (3::Int)  
  tick <- A.readMutVarForCAS mutvar
  dbgPrint 1$"YAY, read the IORef, ticket "++show tick
  dbgPrint 1$"     and the value was:  "++show (peekTicket tick)

  (True,tick2) <- A.casMutVar mutvar tick 99 
  dbgPrint 1$"Hoorah!  Attempted compare and swap..."
--  dbgPrint 1$"         Result was: "++show (True,tick2)

  dbgPrint 1$"Ok, next take a look at a SECOND CAS attempt, to see if the ticket from the first works..."
  res2 <- A.casMutVar mutvar tick2 12345678
  dbgPrint 1$"Result was: "++show res2
  
--  res <- A.casMutVar mutvar tick 99 
  res3 <- A.readMutVarForCAS mutvar
  dbgPrint 1$"To check contents, did a SECOND read: "++show res3

  return ()

---- toddaaro's tests -----

case_create_and_read :: Assertion
case_create_and_read = do
  dbgPrint 1$ "   Creating a single value and trying to read it."
  x <- newIORef (120::Int)
  valf <- readIORef x
  assertBool "   Does x equal 120?" (valf == 120)

case_create_and_mutate :: Assertion
case_create_and_mutate = do
  dbgPrint 1$ "   Creating a single 'ticket' based variable to use and mutating it once."
  x <- newIORef (5::Int)
  tick <- A.readForCAS(x)
  res <- A.casIORef x tick 120
  dbgPrint 1$ "  Did setting it to 120 work?"
  dbgPrint 1$ "  Result was: " ++ show res
  valf <- readIORef x
  assertBool "Does our x equal 120?" (valf == 120)

case_create_and_mutate_twice :: Assertion
case_create_and_mutate_twice = do
  dbgPrint 1$ "  Creating a single 'ticket' based variable to mutate twice."
  x <- newIORef (0::Int)
  tick1 <- A.readForCAS(x)
  res1 <- A.casIORef x tick1 5
  tick2 <- A.readForCAS(x)
  res2 <- A.casIORef x tick2 120
  valf <- readIORef x
  assertBool "Does the value after the first mutate equal 5?" (peekTicket tick2 == 5)
  assertBool "Does the value after the second mutate equal 120?" (valf == 120)


-- [2013.07.19] I just saw an isolated failure of this one:
case_n_threads_mutate :: Assertion
case_n_threads_mutate = do
  dbgPrint 1$ "   Creating 120 threads and having each increment a counter value."
  counter <- newIORef (0::Int)
  let work :: IORef Int -> IO ()
      work = (\counter -> do
                        tick <- A.readForCAS(counter)
                        (b,_) <- A.casIORef counter tick (peekTicket tick + 1)
                        unless b $ work counter)
  arr <- forkJoin 120 (\_ -> work counter) 
  ans <- readIORef counter
  assertBool "Did the sum end up equal to 120?" (ans == 120)

-- | Just make sure these link and run properly:
case_run_barriers :: Assertion
case_run_barriers = do
  A.storeLoadBarrier
  A.loadLoadBarrier
  A.writeBarrier

----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Adapted Old tests from original CAS library:  


-- | First test: Run a simple CAS a small number of times.
test_succeed_once :: (Show a, Num a, Eq a) => a -> Assertion
test_succeed_once n = 
  do
     performGC -- We *ASSUME* GC does not happen below.
     performGC -- We *ASSUME* GC does not happen below.
     checkGCStats
     gc1 <- getGCCount 
     r <- newIORef n
     bitls <- newIORef []
     tick1 <- A.readForCAS r
     let loop 0 = return ()
	 loop n = do
          res <- A.casIORef r tick1 100
          atomicModifyIORef bitls (\x -> (res:x, ()))
--          putStrLn$ "  CAS result: " ++ show res
          loop (n-1)
     loop 10

     x <- readIORef r
     assertEqual "Finished with loop, read cell: " 100 x
     
     writeIORef r 111     
     y <- readIORef r
     assertEqual "Wrote and read again read: " 111 y

     ls <- readIORef bitls
     let rev = (reverse ls)
         tickets = map snd rev
         (hd:tl) = map fst rev

     gc2 <- getGCCount
     if gc1 /= gc2
       then putStrLn " [skipped] test couldn't be assessed properly due to GC."
       else do      
  --     print scrubbed
       assertBool "Only first succeeds" (all (/= hd) tl)
       assertBool "All but first fail" (all (== head tl) (tail tl))
       assertEqual "First should succeed, rest fail"
                   (hd : tl)
                   (True : replicate 9 False)


-- | This version hammers on CASref from all threads, then checks to see
-- if enough threads succeeded enough of the time.
--
-- If each thread tries K attempts, there should be at least K total successes.  To
-- establish this consider the inductive argument.  One thread should succeed all the
-- time.  Adding a second thread can only foil the K attempts of the first thread by
-- itself succeeding (leaving the total at or above K).  Likewise for the third
-- thread and so on.
-- 
-- Conversely, for N threads each aiming to complete K operations,
-- there should be at most N*N*K total operations required.
test_all_hammer_one :: (Show a, Num a, Eq a) => Int -> Int -> a -> Assertion
test_all_hammer_one threads iters seed = do
  ref <- newIORef seed
  logs::[[Bool]] <- forkJoin threads $ \_ -> 
    do checkGCStats
       let loop 0 _ _ !acc = return (reverse acc)
	   loop n !ticket !expected !acc = do
            -- This line will result in boxing/unboxing and using extra memory locations:
--            let bumped = expected + 1 
            bumped <- evaluate$ expected + 1
	    (res,tick) <- casIORef ref ticket bumped
	    case res of
              True -> do
                when (iters < 30) $
                  dbgPrint 1$ "  Succeed CAS, old tick "++show ticket++" new "++show tick++", wrote "++show bumped
                loop (n-1) tick bumped (True:acc)
              False -> do
                let v = peekTicket tick
                when (iters < 30) $
                  dbgPrint 1 $ 
                            "  Fizzled CAS with ticket: "++show ticket ++" containing "++show v++
                            ", expected: "++ show expected ++
                            " (#"++show (unsafeName expected)++"): " 
                            ++ " found " ++ show v ++ " (#"++show (unsafeName v)++", ticket "++show tick++")"
                loop (n-1) tick v      (False:acc)

       tick0 <- readForCAS ref
       loop iters tick0 (peekTicket tick0) []

  numGcs <- getGCCount
  let successes = map (length . filter id) logs
      total_success = sum successes
      bool2char True  = '1'
      bool2char False = '0'
      -- EACH thread may fail on a single GC (in theory)
      expected_success = iters - (threads * fromIntegral numGcs)
      msg = ("Runs "++show (map length logs)++" (GCs "++show numGcs++"), had enough successes?: "
              ++show successes++" >= "++ show expected_success ++"\n"
              ++(unlines $ map (dotdot 80 . ("  "++) . map bool2char) logs) )
  dbgPrint 1 msg
  assertBool msg
             (total_success >= expected_success)


----------------------------------------------------------------------------------------------------
{-

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
-}
