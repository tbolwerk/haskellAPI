{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_resourcet (
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
version = Version [1,2,4,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/rsrct-1.2.4.2-79efd454/bin"
libdir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/rsrct-1.2.4.2-79efd454/lib"
dynlibdir  = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/lib"
datadir    = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/rsrct-1.2.4.2-79efd454/share"
libexecdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/rsrct-1.2.4.2-79efd454/libexec"
sysconfdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/rsrct-1.2.4.2-79efd454/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "resourcet_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "resourcet_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "resourcet_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "resourcet_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "resourcet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "resourcet_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
