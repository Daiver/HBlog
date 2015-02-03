module Paths_HBlog (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/daiver/coding/HBlog/.cabal-sandbox/bin"
libdir     = "/home/daiver/coding/HBlog/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/HBlog-0.1"
datadir    = "/home/daiver/coding/HBlog/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/HBlog-0.1"
libexecdir = "/home/daiver/coding/HBlog/.cabal-sandbox/libexec"
sysconfdir = "/home/daiver/coding/HBlog/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HBlog_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HBlog_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HBlog_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HBlog_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HBlog_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
