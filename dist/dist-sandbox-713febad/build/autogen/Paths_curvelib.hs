module Paths_curvelib (
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
libdir     = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\curvelib-0.1.0.0"
datadir    = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\curvelib-0.1.0.0"
libexecdir = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\curvelib-0.1.0.0"
sysconfdir = "C:\\src\\haskell\\curvelib\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "curvelib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "curvelib_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "curvelib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "curvelib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "curvelib_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
