module Paths_CurveLib (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\bin"
libdir     = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\CurveLib-0.1.0.0"
datadir    = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\CurveLib-0.1.0.0"
libexecdir = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\CurveLib-0.1.0.0"
sysconfdir = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CurveLib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CurveLib_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CurveLib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CurveLib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CurveLib_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
