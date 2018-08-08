module Fetch (tests) where

-- tests for our fetch-and-* family of functions.
import Control.Monad
import System.Random
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test)
import Test.HUnit (assertEqual,assertBool)
import Data.Primitive
import Data.List
import Data.Bits
import Data.Atomics
import Control.Monad.Primitive
import Control.Concurrent

tests :: [Test]
tests = [
      testCase "Fetch-and-* operations return previous value" case_return_previous
    , testCase "Fetch-and-* operations behave like their corresponding bitwise operators" case_like_bitwise
    , testCase "fetchAndIntArray and fetchOrIntArray are atomic"  $ fetchAndOrTest  10000000
    , testCase "fetchNandIntArray atomic"                         $ fetchNandTest   1000000
    , testCase "fetchAddIntArray and fetchSubIntArray are atomic" $ fetchAddSubTest 10000000
    , testCase "fetchXorIntArray is atomic"                       $ fetchXorTest    10000000
    ] 

nand :: Bits a => a -> a -> a
nand x y = complement (x .&. y)

fetchOps :: [( String
            ,  MutableByteArray RealWorld -> Int -> Int -> IO Int
            ,  Int -> Int -> Int )]
fetchOps = [
   ("Add",  fetchAddIntArray,  (+)),
   ("Sub",  fetchSubIntArray,  (-)),
   ("And",  fetchAndIntArray,  (.&.)),
   ("Nand", fetchNandIntArray, nand),
   ("Or",   fetchOrIntArray,   (.|.)),
   ("Xor",  fetchXorIntArray,  xor)
   ]


-- Test all operations at once, somewhat randomly, ensuring they behave like
-- their corresponding bitwise operator; we compose a few operations before
-- inspecting the intermediate result, and spread them randomly around a small
-- array.
-- TODO use quickcheck if we want
case_like_bitwise :: IO ()
case_like_bitwise = do
    let opGroupSize = 5
    let grp n = go n []
          where go _ stck [] = [stck]
                go 0 stck xs = stck : go n [] xs
                go i stck (x:xs) = go (i-1) (x:stck) xs
    -- Inf list of different short sequences of bitwise operations:
    let opGroups = grp opGroupSize $ cycle $ concat $ permutations fetchOps
    
    let size = 4
    randIxs <- randomRs (0, size-1) <$> newStdGen 
    randArgs <- grp opGroupSize . randoms <$> newStdGen
    
    a <- newByteArray (sizeOf (undefined::Int) * size)
    forM_ [0.. size-1] $ \ix-> writeByteArray a ix (0::Int)

    forM_ (take 1000000 $ zip randIxs $ zipWith zip opGroups randArgs) $
        \ (ix, opsArgs)-> do
            assertEqual "test not b0rken" (length opsArgs) opGroupSize
            
            let doOpGroups pureLHS [] = return pureLHS
                doOpGroups pureLHS (((_,atomicOp,op), v) : rest) = do
                    atomicOp a ix v >> doOpGroups (pureLHS `op` v) rest
                  
            vInitial <- readByteArray a ix
            vFinalPure <- doOpGroups vInitial opsArgs
            vFinal <- readByteArray a ix

            let nmsArgs = map (\ ((nm,_,_),v) -> (nm,v)) opsArgs
            assertEqual ("sequence on initial value "++(show vInitial)
                          ++" of ops with RHS args: "++(show nmsArgs)
                          ++" gives same result in both pure and atomic op"
                        ) vFinal vFinalPure

              
            
-- check all operations return the value before the operation was applied;
-- basic smoke test, with each op tested individually.
case_return_previous :: IO ()
case_return_previous = do
    let l = length fetchOps
    a <- newByteArray (sizeOf (undefined::Int) * l)
    let randomInts = take l . randoms <$> newStdGen :: IO [Int]
    initial <- randomInts
    forM_ (zip [0..] initial) $ \(ix, v)-> writeByteArray a ix v

    args <- randomInts
    forM_ (zip4 [0..] initial args fetchOps) $ \(ix, pre, v, (nm,atomicOp,op))-> do
        pre' <- atomicOp a ix v
        assertEqual (fetchStr nm "returned previous value") pre pre'
        let post = pre `op` v
        post' <- readByteArray a ix
        assertEqual (fetchStrArgVal nm v pre "operation was seen correctly on read") post post'

fetchStr :: String -> String -> String
fetchStr nm = (("fetch"++nm++"IntArray: ")++)
fetchStrArgVal :: (Show a, Show a1) => String -> a -> a1 -> String -> String
fetchStrArgVal nm v initial = (("fetch"++nm++"IntArray, with arg "++(show v)++" on value "++(show initial)++": ")++)

-- ----------------------------------------------------------------------------
-- Tests of atomicity:


-- Concurrently run a sequence of AND and OR simultaneously on separate parts
-- of the bit range of an Int.
fetchAndOrTest :: Int -> IO ()
fetchAndOrTest iters = do
    out0 <- newEmptyMVar
    out1 <- newEmptyMVar
    mba <- newByteArray (sizeOf (undefined :: Int))
    let andLowersBit , orRaisesBit :: Int -> Int
        andLowersBit = clearBit (complement 0)
        orRaisesBit = setBit 0
    writeByteArray mba 0 (0 :: Int)
    -- thread 1 toggles bit 0, thread 2 toggles bit 1; then we verify results
    -- in the main thread.
    let go v b = do
            -- Avoid stack overflow on GHC 7.6:
            let replicateMrev l 0 = putMVar v l
                replicateMrev l iter = do
                       low <- fetchOrIntArray mba 0 (orRaisesBit b)
                       high <- fetchAndIntArray mba 0 (andLowersBit b)
                       replicateMrev ((low,high):l) (iter-1)
             in replicateMrev [] iters
    void $ forkIO $ go out0 0
    void $ forkIO $ go out1 1
    res0 <- takeMVar out0
    res1 <- takeMVar out1
    let check b = all ( \(low,high)-> (not $ testBit low b) && testBit high b)

    assertBool "fetchAndOrTest not broken" $ length (res0++res1) == iters*2
    assertBool "fetchAndOrTest thread1" $ check 0 res0
    assertBool "fetchAndOrTest thread2" $ check 1 res1

-- Nand of 1 is a bit complement. Concurrently run two threads running an even
-- number of complements in this way and verify the final value is unchanged.
-- TODO think of a more clever test
fetchNandTest :: Int -> IO ()
fetchNandTest iters = do
    let nandComplements = complement 0
        dblComplement mba = replicateM_ (2 * iters) $
            fetchNandIntArray mba 0 nandComplements
    randomInts <- take 10 . randoms <$> newStdGen :: IO [Int]
    forM_ randomInts $ \ initial -> do
        final <- race initial dblComplement dblComplement 
        assertEqual "fetchNandTest" initial final


-- ----------------------------------------------------------------------------
-- Code below copied with minor modifications from GHC
-- testsuite/tests/concurrent/should_run/AtomicPrimops.hs @ f293931
-- ----------------------------------------------------------------------------


-- | Test fetchAddIntArray# by having two threads concurrenctly
-- increment a counter and then checking the sum at the end.
fetchAddSubTest :: Int -> IO ()
fetchAddSubTest iters = do
    tot <- race 0
        (\ mba -> work fetchAddIntArray mba iters 2)
        (\ mba -> work fetchSubIntArray mba iters 1)
    assertEqual "fetchAddSubTest" iters tot
  where
    work :: (MutableByteArray RealWorld -> Int -> Int -> IO Int) -> MutableByteArray RealWorld -> Int -> Int
         -> IO ()
    work _ _    0 _ = return ()
    work op mba n val = op mba 0 val >> work op mba (n-1) val

-- | Test fetchXorIntArray# by having two threads concurrenctly XORing
-- and then checking the result at the end. Works since XOR is
-- commutative.
--
-- Covers the code paths for AND, NAND, and OR as well.
fetchXorTest :: Int -> IO ()
fetchXorTest iters = do
    res <- race n0
        (\ mba -> work mba iters t1pat)
        (\ mba -> work mba iters t2pat)
    assertEqual "fetchXorTest" expected res
  where
    work :: MutableByteArray RealWorld -> Int -> Int -> IO ()
    work _   0 _ = return ()
    work mba n val = fetchXorIntArray mba 0 val >> work mba (n-1) val

    -- Initial value is a large prime and the two patterns are 1010...
    -- and 0101...
    (n0, t1pat, t2pat)
        -- TODO: If we want to silence warnings from here, use CPP conditional
        --       on arch x86_64
        | sizeOf (undefined :: Int) == 8 =
            (0x00000000ffffffff, 0x5555555555555555, 0x9999999999999999)
        | otherwise = (0x0000ffff, 0x55555555, 0x99999999)
    expected
        | sizeOf (undefined :: Int) == 8 = 4294967295
        | otherwise = 65535

-- | Create two threads that mutate the byte array passed to them
-- concurrently. The array is one word large.
race :: Int                    -- ^ Initial value of array element
     -> (MutableByteArray RealWorld -> IO ())  -- ^ Thread 1 action
     -> (MutableByteArray RealWorld -> IO ())  -- ^ Thread 2 action
     -> IO Int                 -- ^ Final value of array element
race n0 thread1 thread2 = do
    done1 <- newEmptyMVar
    done2 <- newEmptyMVar
    mba <- newByteArray (sizeOf (undefined :: Int))
    writeByteArray mba 0 n0
    void $ forkIO $ thread1 mba >> putMVar done1 ()
    void $ forkIO $ thread2 mba >> putMVar done2 ()
    mapM_ takeMVar [done1, done2]
    readByteArray mba 0
