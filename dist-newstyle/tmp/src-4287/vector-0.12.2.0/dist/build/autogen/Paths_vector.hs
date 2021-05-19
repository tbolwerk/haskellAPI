{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_vector (
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
version = Version [0,12,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/vctr-0.12.2.0-71d43fbb/bin"
libdir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/vctr-0.12.2.0-71d43fbb/lib"
dynlibdir  = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/lib"
datadir    = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/vctr-0.12.2.0-71d43fbb/share"
libexecdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/vctr-0.12.2.0-71d43fbb/libexec"
sysconfdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/vctr-0.12.2.0-71d43fbb/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vector_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vector_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vector_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vector_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vector_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vector_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
