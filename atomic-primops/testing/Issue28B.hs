
import CommonTesting  (dbgPrint)

import Data.Atomics as A
import GHC.STRef
import GHC.IORef

-- The Int version works, the String version fails:
-- old = (3::Int); new = 99
old = "hi"; new = "bye"


main = do
--  dbgPrint 1 "\nUsing new 'ticket' based compare and swap:"

  IORef (STRef mutvar) <- newIORef old
  tick <- A.readMutVarForCAS mutvar
--  dbgPrint 1$"YAY, read the IORef, ticket "++show tick
--  dbgPrint 1$"     and the value was:  "++show (peekTicket tick)

  (True,tick2) <- A.casMutVar mutvar tick new
--  dbgPrint 1$"Hoorah!  Attempted compare and swap..."
{-
--  dbgPrint 1$"Ok, next take a look at a SECOND CAS attempt, to see if the ticket from the first works..."
  res2 <- A.casMutVar mutvar tick2 12345678
--  dbgPrint 1$"Result was: "++show res2
  
  res3 <- A.readMutVarForCAS mutvar
--  dbgPrint 1$"To check contents, did a SECOND read: "++show res3
-}
  return ()


