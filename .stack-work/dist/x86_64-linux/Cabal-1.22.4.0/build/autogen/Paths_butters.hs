module Paths_butters (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/michael/dev/haskell/butters/.stack-work/install/x86_64-linux/lts-3.13/7.10.2/bin"
libdir     = "/home/michael/dev/haskell/butters/.stack-work/install/x86_64-linux/lts-3.13/7.10.2/lib/x86_64-linux-ghc-7.10.2/butters-0.1.0.0-BsvUj3aWzgA1bKCFsnr7Vo"
datadir    = "/home/michael/dev/haskell/butters/.stack-work/install/x86_64-linux/lts-3.13/7.10.2/share/x86_64-linux-ghc-7.10.2/butters-0.1.0.0"
libexecdir = "/home/michael/dev/haskell/butters/.stack-work/install/x86_64-linux/lts-3.13/7.10.2/libexec"
sysconfdir = "/home/michael/dev/haskell/butters/.stack-work/install/x86_64-linux/lts-3.13/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "butters_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "butters_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "butters_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "butters_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "butters_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
