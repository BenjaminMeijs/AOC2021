{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_adventOfCode2021 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\adventOfCode2021-0.1.0.0-inplace-adventOfCode2021"
dynlibdir  = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2"
datadir    = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\adventOfCode2021-0.1.0.0"
libexecdir = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\adventOfCode2021-0.1.0.0-inplace-adventOfCode2021\\x86_64-windows-ghc-8.10.2\\adventOfCode2021-0.1.0.0"
sysconfdir = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "adventOfCode2021_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "adventOfCode2021_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "adventOfCode2021_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "adventOfCode2021_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adventOfCode2021_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adventOfCode2021_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)