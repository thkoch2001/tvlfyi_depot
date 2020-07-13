{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_regex_tdfa_text (
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
version = Version [1,0,0,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/nix/store/zi0mfv5cq81l6nd86p8yg1yldgidlkgq-regex-tdfa-text-1.0.0.3/bin"
libdir     = "/nix/store/zi0mfv5cq81l6nd86p8yg1yldgidlkgq-regex-tdfa-text-1.0.0.3/lib/ghc-8.8.3/x86_64-linux-ghc-8.8.3/regex-tdfa-text-1.0.0.3-6AtTFS4ZWfb2PfPZ50w3rd"
dynlibdir  = "/nix/store/zi0mfv5cq81l6nd86p8yg1yldgidlkgq-regex-tdfa-text-1.0.0.3/lib/ghc-8.8.3/x86_64-linux-ghc-8.8.3"
datadir    = "/nix/store/zi0mfv5cq81l6nd86p8yg1yldgidlkgq-regex-tdfa-text-1.0.0.3/share/x86_64-linux-ghc-8.8.3/regex-tdfa-text-1.0.0.3"
libexecdir = "/nix/store/zi0mfv5cq81l6nd86p8yg1yldgidlkgq-regex-tdfa-text-1.0.0.3/libexec/x86_64-linux-ghc-8.8.3/regex-tdfa-text-1.0.0.3"
sysconfdir = "/nix/store/zi0mfv5cq81l6nd86p8yg1yldgidlkgq-regex-tdfa-text-1.0.0.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "regex_tdfa_text_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "regex_tdfa_text_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "regex_tdfa_text_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "regex_tdfa_text_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "regex_tdfa_text_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "regex_tdfa_text_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
