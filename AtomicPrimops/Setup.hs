
import Control.Monad (when)
import Language.Haskell.TH
import Distribution.Simple                (defaultMainWithHooks, simpleUserHooks, UserHooks(postConf), Args)
import Distribution.Simple.Utils          (cabalVersion)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup          (ConfigFlags)
import Distribution.Version               (Version(..))
import Distribution.PackageDescription    (PackageDescription)
import Debug.Trace

checkGoodVersion :: IO ()
checkGoodVersion =
  if   cabalVersion >= Version [1,17,0] []
  then putStrLn (" [Setup.hs] This version of Cabal is ok for profiling: "++show cabalVersion)
  else error (" [Setup.hs] This package should not be used in profiling mode with cabal version "++
                        show (versionBranch cabalVersion)++" < 1.17.0\n"++
                        " It will break, see cabal issue #1284")

main :: IO ()
main = do
  let myPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
      myPostConf _args confFlags _descr lbi =
        when (withProfLib lbi)
          checkGoodVersion
      hooks = simpleUserHooks { postConf = myPostConf }
  defaultMainWithHooks hooks
