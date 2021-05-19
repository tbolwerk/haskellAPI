{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_text_short (
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
version = Version [0,1,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/txt-shrt-0.1.3-3093729a/bin"
libdir     = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/txt-shrt-0.1.3-3093729a/lib"
dynlibdir  = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/lib"
datadir    = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/txt-shrt-0.1.3-3093729a/share"
libexecdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/txt-shrt-0.1.3-3093729a/libexec"
sysconfdir = "/Users/twanbolwerk/.cabal/store/ghc-8.6.5/txt-shrt-0.1.3-3093729a/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "text_short_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "text_short_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "text_short_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "text_short_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_short_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "text_short_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
