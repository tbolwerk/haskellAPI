{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_network (
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
version = Version [3,1,2,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ntwrk-3.1.2.1-8a45affc/bin"
libdir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ntwrk-3.1.2.1-8a45affc/lib"
dynlibdir  = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/lib"
datadir    = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ntwrk-3.1.2.1-8a45affc/share"
libexecdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ntwrk-3.1.2.1-8a45affc/libexec"
sysconfdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/ntwrk-3.1.2.1-8a45affc/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "network_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "network_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "network_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "network_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "network_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "network_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
