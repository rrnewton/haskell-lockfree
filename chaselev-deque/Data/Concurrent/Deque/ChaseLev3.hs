{-# LANGUAGE FlexibleInstances, NamedFieldPuns, CPP, ScopedTypeVariables, BangPatterns, MagicHash #-}

-- | Here is an alternate, fresh implementation ported from the GHC rts sources.

module Data.Concurrent.Deque.ChaseLev3

       where 

import Data.IORef
import Data.Bits
import Data.Word
import Data.List (isInfixOf, intersperse)
import Data.Atomics (storeLoadBarrier)
import Data.Atomics.Counter.Reference
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Exception (catch, SomeException, throw, evaluate,try, assert)
import Control.Monad.Primitive (PrimState)

{-# INLINE rd #-}
{-# INLINE wr #-}
{-# INLINE nu #-}
{-# INLINE cpy #-}
{-# INLINE slc #-}

type Vec a = MV.MVector (PrimState IO) a

rd  :: Vec a -> Int -> IO a
nu  :: Int -> IO (Vec a)
cpy :: Vec a -> Vec a -> IO ()
wr  :: Vec a -> Int -> a -> IO ()

#define DEBUGCL
#ifndef DEBUGCL
dbg = False
nu  = MV.unsafeNew
rd  = MV.unsafeRead
wr  = MV.unsafeWrite
slc = MV.unsafeSlice
cpy = MV.unsafeCopy
#else
#warning "Activating DEBUGCL!"
dbg = True
nu  = MV.new 
rd  = MV.read
slc = MV.slice
cpy = MV.copy
wr  = MV.write
-- Temp, debugging: Our own bounds checking, better error:
-- wr v i x = 
--   if i >= MV.length v
--   then error (printf "ERROR: Out of bounds of top of vector index %d, vec length %d\n" i (MV.length v))
--   else MV.write v i x

-- [2013.06.25] Note Issue5 is not affected by this:
-- {-# NOINLINE pushL #-}
-- {-# NOINLINE tryPopL #-}
-- {-# NOINLINE tryPopR #-}
#endif

--------------------------------------------------------------------------------

data WSDeque a = WSDeque {
    -- Size of elements array. Used for modulo calculation: we round up
    -- to powers of 2 and use the dyadic log (modulo == bitwise &) 
    size       :: {-# UNPACK #-} !(IORef Word), 
    moduloSize :: {-# UNPACK #-} !(IORef Word), -- bitmask for modulo 

    -- top, index where multiple readers steal() (protected by a cas)
    top    :: {-# UNPACK #-} !AtomicCounter, -- Volatile

    -- bottom, index of next free place where one writer can push
    -- elements. This happens unsynchronised.
    bottom :: {-# UNPACK #-} !AtomicCounter,  -- volatile

    -- both top and bottom are continuously incremented, and used as
    -- an index modulo the current array size.
  
    -- lower bound on the current top value. This is an internal
    -- optimisation to avoid unnecessarily accessing the top field
    -- inside pushBottom
    topBound :: {-# UNPACK #-} !AtomicCounter, -- volatile 

    -- The elements array
    elements :: {-# UNPACK #-} !(IORef (MV.IOVector a))

  }


{- INVARIANTS, in this order: reasonable size,
   topBound consistent, space pointer, space accessible to us.
   
   NB. This is safe to use only (a) on a deque owned by the
   current thread, or (b) when there's only one thread running, or no
   stealing going on (e.g. during GC).
-}
assert_wsdeque_invariants WSDeque{size,moduloSize,top,bottom,topBound,elements} = do
  top'      <- readCounter top
  bottom'   <- readCounter bottom
  topBound' <- readCounter topBound
  size'     <- readIORef size
  elements' <- readIORef elements
  assert (size' > 0) $ 
   assert (topBound' <= top') $
    return ()
--  ASSERT((p)->elements != NULL);  
--  ASSERT(*((p)->elements) || 1);
  _ <- rd elements' 0
  _ <- rd elements' (fromIntegral size' - 1)
  return ()


dbgInspectWSDeque :: Show a => WSDeque a -> IO String
dbgInspectWSDeque WSDeque{top,bottom,elements} = do
  tp <- readCounter top
  bt <- readCounter bottom
  vc <- readIORef elements
  elems  <- fmap V.toList$ V.freeze vc
  elems' <- mapM safePrint elems
  let sz = MV.length vc
  return$ "  {DbgInspectWSDeque: top "++show tp++", bot "++show bt++", arr size "++show sz++"\n" ++
--          "   " 
          "   [ "++(concat $ intersperse " " elems')++" ]\n"++
          "  end_DbgInspectWSDeque}"
 where
   -- Print any thunk, even if it raises an exception.
   safePrint :: Show a => a -> IO String
   safePrint val = do
     res <- try (evaluate val)
     case res of
       Left (e::SomeException)
         | isInfixOf "uninitialised element" (show e) -> return "<uninit>"
         | otherwise -> return$ "<"++ show e ++">"
       Right val' -> return (show val')

-- No: it is possible that top > bottom when using pop()
--  ASSERT((p)->bottom >= (p)->top);           
--  ASSERT((p)->size > (p)->bottom - (p)->top);

{- -----------------------------------------------------------------------------
 * Operations
 *
 * A WSDeque has an *owner* thread.  The owner can perform any operation;
 * other threads are only allowed to call stealWSDeque_(),
 * stealWSDeque(), looksEmptyWSDeque(), and dequeElements().
 *
 * -------------------------------------------------------------------------- -}

-- | Allocation
newWSDeque :: Int -> IO (WSDeque a)

-- | Take an element from the "write" end of the pool.  Can be called
-- by the pool owner only.
popWSDeque :: WSDeque a -> IO (Maybe a)

-- | Push onto the "write" end of the pool.  Return true if the push
-- succeeded, or false if the deque is full.
pushWSDeque :: WSDeque a -> a -> IO Bool
pushWSDeque = undefined

-- Removes all elements from the deque
discardElements :: WSDeque a -> IO ()

-- Removes an element of the deque from the "read" end, or returns
-- NULL if the pool is empty, or if there was a collision with another
-- thief.
stealWSDeque_ :: WSDeque a -> IO (Maybe a)
stealWSDeque_ = undefined 

-- Removes an element of the deque from the "read" end, or returns
-- NULL if the pool is empty.
stealWSDeque :: WSDeque a -> IO (Maybe a)
stealWSDeque = undefined 

-- "guesses" whether a deque is empty. Can return false negatives in
--  presence of concurrent steal() calls, and false positives in
--  presence of a concurrent pushBottom().
looksEmptyWSDeque :: WSDeque a -> IO Bool

dequeElements ::  WSDeque a -> IO Int


{- -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- -}

dequeElements WSDeque{top,bottom} = do
    t <- readCounter top
    b <- readCounter bottom
    -- try to prefer false negatives by reading top first
    return (b - t)

looksEmptyWSDeque q = do
  s <- dequeElements q
  return (s <= 0)

discardElements WSDeque{top,bottom} = do
  b <- readCounter bottom
  writeCounter top b
--    pool->topBound = pool->top;

{- -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * Work-stealing Deque data structure
 * 
 * The implementation uses Double-Ended Queues with lock-free access
 * (thereby often called "deque") as described in
 *
 * D.Chase and Y.Lev, Dynamic Circular Work-Stealing Deque.
 * SPAA'05, July 2005, Las Vegas, USA.
 * ACM 1-58113-986-1/05/0007
 *
 * Author: Jost Berthold MSRC 07-09/2008
 *
 * The DeQue is held as a circular array with known length. Positions
 * of top (read-end) and bottom (write-end) always increase, and the
 * array is accessed with indices modulo array-size. While this bears
 * the risk of overflow, we assume that (with 64 bit indices), a
 * program must run very long to reach that point.
 * 
 * The write end of the queue (position bottom) can only be used with
 * mutual exclusion, i.e. by exactly one caller at a time.  At this
 * end, new items can be enqueued using pushBottom()/newSpark(), and
 * removed using popBottom()/reclaimSpark() (the latter implying a cas
 * synchronisation with potential concurrent readers for the case of
 * just one element).
 * 
 * Multiple readers can steal from the read end (position top), and
 * are synchronised without a lock, based on a cas of the top
 * position. One reader wins, the others return NULL for a failure.
 * 
 * Both popWSDeque and stealWSDeque also return NULL when the queue is empty.
 *
 * Testing: see testsuite/tests/rts/testwsdeque.c.  If
 * there's anything wrong with the deque implementation, this test
 * will probably catch it.
 * 
 * ----------------------------------------------------------------------------}

-- define CASTOP(addr,old,new) ((old) == cas(((StgPtr)addr),(old),(new)))


{- -----------------------------------------------------------------------------
 * newWSDeque
 * -------------------------------------------------------------------------- -}

{- internal helpers ... -}

-- Crop all but the leading bit of a number.
roundUp2 :: Word -> Word
roundUp2 origval = do  
  -- Word is unsigned anyway, only catch 0 
  if origval == 0 then
    error "DeQue,roundUp2: invalid size 0 requested"
  else 
    {- at least 1 bit set, shift up to its place -}
    let loop !rounded !val = 
          let val' = shiftR val 1 in
          if val' == 0 
          then rounded
          else loop (shiftL rounded 1) val'
    in loop 1 origval

newWSDeque size = do   
    let realsize = roundUp2 (fromIntegral size) {- to compute modulo as a bitwise & -} 
    arr <- nu size

    top      <- newCounter 
    bottom   <- newCounter
    topBound <- newCounter
    size       <- newIORef realsize
    moduloSize <- newIORef (realsize - 1)
    elements   <- newIORef arr
    let q = WSDeque {
              top, bottom, 
              topBound,   {- read by writer, updated each time top is read -}
              size,       {- power of 2 -}
              moduloSize, {- n % size == n & moduloSize  -}
              elements
            }
    assert_wsdeque_invariants q
    return q

{- -----------------------------------------------------------------------------
 * 
 * popWSDeque: remove an element from the write end of the queue.
 * Returns the removed spark, and NULL if a race is lost or the pool
 * empty.
 *
 * If only one spark is left in the pool, we synchronise with
 * concurrently stealing threads by using cas to modify the top field.
 * This routine should NEVER be called by a task which does not own
 * this deque.
 *
 * -------------------------------------------------------------------------- -}

popWSDeque q@WSDeque{top,bottom,topBound,moduloSize,elements} = do  
    {- also a bit tricky, has to avoid concurrent steal() calls by
       accessing top with cas, when there is only one element left -}
    
    assert_wsdeque_invariants q
    
    b <- readCounter bottom 

    -- "decrement b as a test, see what happens"
    b <- evaluate (b - 1)
    writeCounter bottom b

    -- very important that the following read of q->top does not occur
    -- before the earlier write to q->bottom.    
    storeLoadBarrier

    tt <- readCounterForCAS top
          {- using topBound would give an *upper* bound, we
             need a lower bound. We use the real top here, but
             can update the topBound value -}
    let t = peekCTicket tt
    writeCounter topBound t

    let currSize = b - t
    if currSize < 0 then do {- was empty before decrementing b, set b
                                 consistently and abort -}
        writeCounter bottom t;
        return Nothing
     else do    
       ms  <- readIORef moduloSize
       arr <- readIORef elements
       -- read the element at b      
       removed <- rd arr (fromIntegral ((fromIntegral b) .&. ms))
       
       if currSize > 0 then {- no danger, still elements in buffer after b-- -}
           -- debugBelch("popWSDeque: t=%ld b=%ld = %ld\n", t, b, removed);
           return (Just removed)
        else do 
         {- otherwise, has someone meanwhile stolen the same (last) element?
            Check and increment top value to know  -}
         (bl,_) <- casCounter top tt (t+1)

         writeCounter bottom   (t+1) {- anyway, empty now. Adjust bottom consistently. -}
         writeCounter topBound (t+1) {- ...and cached top value as well -}

         assert_wsdeque_invariants q
         b' <- readCounter bottom
         t' <- readCounter top
         assert (b' >= t') $ 
          -- debugBelch("popWSDeque: t=%ld b=%ld = %ld\n", t, b, removed);
          return $ if not bl
                   then Nothing {- no success, but we adjusted bottom anyway -}
                   else Just removed
         
{-

{- -----------------------------------------------------------------------------
 * stealWSDeque
 * -------------------------------------------------------------------------- -}

void *
stealWSDeque_ (WSDeque *q)
{
    void * stolen;
    StgWord b,t; 
    
-- Can't do this on someone else's spark pool:
-- ASSERT_WSDEQUE_INVARIANTS(q); 
    
    -- NB. these loads must be ordered, otherwise there is a race
    -- between steal and pop.
    t = q->top;
    load_load_barrier();
    b = q->bottom;
    
    -- NB. b and t are unsigned; we need a signed value for the test
    -- below, because it is possible that t > b during a
    -- concurrent popWSQueue() operation.
    if ((long)b - (long)t <= 0 ) { 
        return NULL; {- already looks empty, abort -}
  }
    
    {- now access array, see pushBottom() -}
    stolen = q->elements[t & q->moduloSize];
    
    {- now decide whether we have won -}
    if ( !(CASTOP(&(q->top),t,t+1)) ) {
        {- lost the race, someon else has changed top in the meantime -}
        return NULL;
    }  {- else: OK, top has been incremented by the cas call -}

    -- debugBelch("stealWSDeque_: t=%d b=%d\n", t, b);

-- Can't do this on someone else's spark pool:
-- ASSERT_WSDEQUE_INVARIANTS(q); 
    
    return stolen;
}

void *
stealWSDeque (WSDeque *q)
{
    void *stolen;
    
    do { 
        stolen = stealWSDeque_(q);
    } while (stolen == NULL && !looksEmptyWSDeque(q));
    
    return stolen;
}

{- -----------------------------------------------------------------------------
 * pushWSQueue
 * -------------------------------------------------------------------------- -}

define DISCARD_NEW

{- enqueue an element. Should always succeed by resizing the array
   (not implemented yet, silently fails in that case). -}
rtsBool
pushWSDeque (WSDeque* q, void * elem)
{
    StgWord t;
    StgWord b;
    StgWord sz = q->moduloSize; 
    
    ASSERT_WSDEQUE_INVARIANTS(q); 
    
    {- we try to avoid reading q->top (accessed by all) and use
       q->topBound (accessed only by writer) instead. 
       This is why we do not just call empty(q) here.
    -}
    b = q->bottom;
    t = q->topBound;
    if ( (StgInt)b - (StgInt)t >= (StgInt)sz ) { 
        {- NB. 1. sz == q->size - 1, thus ">="
           2. signed comparison, it is possible that t > b
        -}
        {- could be full, check the real top value in this case -}
        t = q->top;
        q->topBound = t;
        if (b - t >= sz) { {- really no space left :-( -}
            {- reallocate the array, copying the values. Concurrent steal()s
               will in the meantime use the old one and modify only top.
               This means: we cannot safely free the old space! Can keep it
               on a free list internally here...
               
               Potential bug in combination with steal(): if array is
               replaced, it is unclear which one concurrent steal operations
               use. Must read the array base address in advance in steal().
            -}
#if defined(DISCARD_NEW)
            ASSERT_WSDEQUE_INVARIANTS(q); 
            return rtsFalse; -- we didn't push anything
#else
            {- could make room by incrementing the top position here.  In
             * this case, should use CASTOP. If this fails, someone else has
             * removed something, and new room will be available.
             -}
            ASSERT_WSDEQUE_INVARIANTS(q); 
#endif
        }
    }

    q->elements[b & sz] = elem;
    {-
       KG: we need to put write barrier here since otherwise we might
       end with elem not added to q->elements, but q->bottom already
       modified (write reordering) and with stealWSDeque_ failing
       later when invoked from another thread since it thinks elem is
       there (in case there is just added element in the queue). This
       issue concretely hit me on ARMv7 multi-core CPUs
     -}
    write_barrier();
    q->bottom = b + 1;
    
    ASSERT_WSDEQUE_INVARIANTS(q); 
    return rtsTrue;
}

-}
