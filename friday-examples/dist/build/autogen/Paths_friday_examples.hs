{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_friday_examples (
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
version = Version [0,1,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ldercher/Desktop/friday-examples/.cabal-sandbox/bin"
libdir     = "/home/ldercher/Desktop/friday-examples/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/friday-examples-0.1.0.1"
dynlibdir  = "/home/ldercher/Desktop/friday-examples/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/ldercher/Desktop/friday-examples/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/friday-examples-0.1.0.1"
libexecdir = "/home/ldercher/Desktop/friday-examples/.cabal-sandbox/libexec"
sysconfdir = "/home/ldercher/Desktop/friday-examples/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "friday_examples_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "friday_examples_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "friday_examples_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "friday_examples_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "friday_examples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "friday_examples_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
