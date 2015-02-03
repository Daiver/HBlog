module Paths_TestPackage (
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
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/daiver/coding/HBlog/.cabal-sandbox/bin"
libdir     = "/home/daiver/coding/HBlog/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/TestPackage-0.0"
datadir    = "/home/daiver/coding/HBlog/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/TestPackage-0.0"
libexecdir = "/home/daiver/coding/HBlog/.cabal-sandbox/libexec"
sysconfdir = "/home/daiver/coding/HBlog/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TestPackage_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TestPackage_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TestPackage_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TestPackage_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TestPackage_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
