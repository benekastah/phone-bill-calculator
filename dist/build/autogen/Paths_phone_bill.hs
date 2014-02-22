module Paths_phone_bill (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/benekastah/Library/Haskell/ghc-7.6.3/lib/phone-bill-0.1.0.0/bin"
libdir     = "/Users/benekastah/Library/Haskell/ghc-7.6.3/lib/phone-bill-0.1.0.0/lib"
datadir    = "/Users/benekastah/Library/Haskell/ghc-7.6.3/lib/phone-bill-0.1.0.0/share"
libexecdir = "/Users/benekastah/Library/Haskell/ghc-7.6.3/lib/phone-bill-0.1.0.0/libexec"
sysconfdir = "/Users/benekastah/Library/Haskell/ghc-7.6.3/lib/phone-bill-0.1.0.0/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "phone_bill_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "phone_bill_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "phone_bill_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "phone_bill_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "phone_bill_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
