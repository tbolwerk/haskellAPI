{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_old_time (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,1,0,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ld-tm-1.1.0.3-412d4789/bin"
libdir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ld-tm-1.1.0.3-412d4789/lib"
dynlibdir  = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/lib"
datadir    = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ld-tm-1.1.0.3-412d4789/share"
libexecdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ld-tm-1.1.0.3-412d4789/libexec"
sysconfdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ld-tm-1.1.0.3-412d4789/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "old_time_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "old_time_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "old_time_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "old_time_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "old_time_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "old_time_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
