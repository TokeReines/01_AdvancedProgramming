{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_arithmetic (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/julianwulff/Documents/Datalogi_MSc/01_AdvancedProgramming/Assignments/01/code/part2/.stack-work/install/x86_64-osx/7a6204206012a4486e02bd88465dd169288096fe02ddb4741f56f89898b1b1bc/8.10.6/bin"
libdir     = "/Users/julianwulff/Documents/Datalogi_MSc/01_AdvancedProgramming/Assignments/01/code/part2/.stack-work/install/x86_64-osx/7a6204206012a4486e02bd88465dd169288096fe02ddb4741f56f89898b1b1bc/8.10.6/lib/x86_64-osx-ghc-8.10.6/arithmetic-0.0.0-3wxLZ1Gl0wSSQRJbhdF7k-my-test-suite"
dynlibdir  = "/Users/julianwulff/Documents/Datalogi_MSc/01_AdvancedProgramming/Assignments/01/code/part2/.stack-work/install/x86_64-osx/7a6204206012a4486e02bd88465dd169288096fe02ddb4741f56f89898b1b1bc/8.10.6/lib/x86_64-osx-ghc-8.10.6"
datadir    = "/Users/julianwulff/Documents/Datalogi_MSc/01_AdvancedProgramming/Assignments/01/code/part2/.stack-work/install/x86_64-osx/7a6204206012a4486e02bd88465dd169288096fe02ddb4741f56f89898b1b1bc/8.10.6/share/x86_64-osx-ghc-8.10.6/arithmetic-0.0.0"
libexecdir = "/Users/julianwulff/Documents/Datalogi_MSc/01_AdvancedProgramming/Assignments/01/code/part2/.stack-work/install/x86_64-osx/7a6204206012a4486e02bd88465dd169288096fe02ddb4741f56f89898b1b1bc/8.10.6/libexec/x86_64-osx-ghc-8.10.6/arithmetic-0.0.0"
sysconfdir = "/Users/julianwulff/Documents/Datalogi_MSc/01_AdvancedProgramming/Assignments/01/code/part2/.stack-work/install/x86_64-osx/7a6204206012a4486e02bd88465dd169288096fe02ddb4741f56f89898b1b1bc/8.10.6/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arithmetic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arithmetic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "arithmetic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "arithmetic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arithmetic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arithmetic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
