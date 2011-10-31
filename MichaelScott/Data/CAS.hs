{-
Copyright (c)2011, Adam Foltzer

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Adam C. Foltzer nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.CAS where

import GHC.IO
import GHC.IORef
import GHC.Prim
import GHC.ST
import GHC.STRef


-- | Performs a machine-level compare and swap operation on an
-- 'STRef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'STRef'.
casSTRef :: STRef s a -- ^ The 'STRef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> ST s (Bool, a) 
casSTRef (STRef var#) old new = ST $ \s1# ->
    case casMutVar# var# old new s1# of
      (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

-- | Performs a machine-level compare and swap operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'IORef'.
casIORef :: IORef a -- ^ The 'IORef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (Bool, a) 
casIORef (IORef var) old new = stToIO (casSTRef var old new)
      

