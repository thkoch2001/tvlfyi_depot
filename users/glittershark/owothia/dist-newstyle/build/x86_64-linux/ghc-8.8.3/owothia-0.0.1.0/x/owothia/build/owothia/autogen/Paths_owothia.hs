{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_owothia (
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
version = Version [0,0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/grfn/.cabal/bin"
libdir     = "/home/grfn/.cabal/lib/x86_64-linux-ghc-8.8.3/owothia-0.0.1.0-inplace-owothia"
dynlibdir  = "/home/grfn/.cabal/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/grfn/.cabal/share/x86_64-linux-ghc-8.8.3/owothia-0.0.1.0"
libexecdir = "/home/grfn/.cabal/libexec/x86_64-linux-ghc-8.8.3/owothia-0.0.1.0"
sysconfdir = "/home/grfn/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "owothia_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "owothia_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "owothia_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "owothia_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "owothia_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "owothia_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
