
import Control.Monad (when)
import Distribution.Simple                (defaultMainWithHooks, simpleUserHooks, UserHooks(postConf), Args)
import Distribution.Simple.Utils          (cabalVersion)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup          (ConfigFlags)
import Distribution.Version               (Version, mkVersion, versionNumbers)
import Distribution.PackageDescription    (PackageDescription)

-- I couldn't figure out a way to do this check from the cabal file, so we drop down
-- here to do it instead:
checkGoodVersion :: IO ()
checkGoodVersion =
  if   cabalVersion >= mkVersion [1,17,0]
  then putStrLn (" [Setup.hs] This version of Cabal is ok for profiling: "++show cabalVersion)
  else error (" [Setup.hs] This package should not be used in profiling mode with cabal version "++
                        show (versionNumbers cabalVersion)++" < 1.17.0\n"++
                        " It will break, see cabal issue #1284")

main :: IO ()
main = do
  let myPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
      myPostConf _args confFlags _descr lbi =
        when (withProfLib lbi)
          checkGoodVersion
      hooks = simpleUserHooks { postConf = myPostConf }
  defaultMainWithHooks hooks
