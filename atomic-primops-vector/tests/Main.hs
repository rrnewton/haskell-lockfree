
import Control.Monad
import Data.Vector
import Data.Vector.Mutable
import Data.Atomics.Vector

main :: IO ()
main = do
  v <- new 10 :: IO (IOVector Int)
  set v 0
  tik <- readVectorElem v 5
  casVectorElem v 5 tik 99
  v' <- unsafeFreeze v  
  print v'
--  assert
  unless (show v' == "fromList [0,0,0,0,0,99,0,0,0,0]")
    (error "Unexpected result!")

    
  
