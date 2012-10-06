{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, 
    MagicHash, UnboxedTuples, UnliftedFFITypes
 #-}
{-# LANGUAGE RankNTypes #-}

module Data.Atomics.Internal (casArray# ) where 

import GHC.Prim
import GHC.Base (Int(I#))
import Data.Word
import Data.Int

-- | Unsafe, machine-level atomic compare and swap on an element within an Array.  
-- foreign import prim "stg_casArrayzh" casArray#
--  :: forall a s . 
--     MutableArray# s a -> Int# -> a -> a -> 
--     State# s -> (# State# s, Int#, a #)    
--   out_of_line = True
--   has_side_effects = True
  
-- A silly fake type to make GHC's foreign prim mechanism happy:
-- data Nonsense

-- I can't use the polymorphic version, so... uh. I guess I can do
-- this and cast?
foreign import prim "stg_casArrayzh" casArrayTypeErased#
  -- :: MutableArray# Nonsense Nonsense -> Int# -> Word# -> Word# -> 
  --    State# Nonsense -> (# State# Nonsense, Int#, Nonsense #) 

  :: MutableArray# () () -> Int# -> Any () -> Any () -> 
     State# () -> (# State# (), Int#, Any () #) 
-- The above type only works in GHC 7.6!!!  We want 7.4 support.


casArray# :: MutableArray# s a -> Int# -> a -> a -> 
             State# s -> (# State# s, Int#, a #)    
casArray# = unsafeCoerce# casArrayTypeErased#


foreign import prim "add1Op" add1#
  :: Int# -> Int# 
  
-- primop CasArrayOp  "casArray#" GenPrimOp
--    MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
--    {Unsafe, machine-level atomic compare and swap on an element within an Array.}
--    with
--    out_of_line = True
--    has_side_effects = True


--  section "Byte Arrays"
--         {Operations on {\tt ByteArray\#}. A {\tt ByteArray\#} is a just a region of
-- diff --git a/includes/stg/MiscClosures.h b/includes/stg/MiscClosures.h
-- index 4fed346..6ffb9af 100644
-- --- a/includes/stg/MiscClosures.h
-- +++ b/includes/stg/MiscClosures.h
-- @@ -378,6 +378,7 @@ RTS_FUN_DECL(stg_word64ToIntegerzh);
--  #endif
 
--  RTS_FUN_DECL(stg_unsafeThawArrayzh);
-- +RTS_FUN_DECL(stg_casArrayzh);
--  RTS_FUN_DECL(stg_newByteArrayzh);
--  RTS_FUN_DECL(stg_newPinnedByteArrayzh);
--  RTS_FUN_DECL(stg_newAlignedPinnedByteArrayzh);
-- diff --git a/rts/Linker.c b/rts/Linker.c
-- index 887cacc..dbcd6ea 100644
-- --- a/rts/Linker.c
-- +++ b/rts/Linker.c
-- @@ -867,6 +867,7 @@ typedef struct _RtsSymbolVal {
--        SymI_HasProto(stg_myThreadIdzh)                   \
--        SymI_HasProto(stg_labelThreadzh)                  \
--        SymI_HasProto(stg_newArrayzh)                     \
-- +      SymI_HasProto(stg_casArrayzh)                     \
--        SymI_HasProto(stg_newArrayArrayzh)                     \
--        SymI_HasProto(stg_newBCOzh)                       \
--        SymI_HasProto(stg_newByteArrayzh)                 \
-- diff --git a/rts/PrimOps.cmm b/rts/PrimOps.cmm
-- index aaedabb..8ab90d7 100644

main2 = do 
  putStrLn "hi "
  print (I# (add1# 3#))

