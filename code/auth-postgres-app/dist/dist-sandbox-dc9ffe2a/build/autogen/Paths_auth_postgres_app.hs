module Paths_auth_postgres_app (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/auth-postgres-app/.cabal-sandbox/bin"
libdir     = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/auth-postgres-app/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/auth-postgres-app-0.1"
datadir    = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/auth-postgres-app/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/auth-postgres-app-0.1"
libexecdir = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/auth-postgres-app/.cabal-sandbox/libexec"
sysconfdir = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/auth-postgres-app/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "auth_postgres_app_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "auth_postgres_app_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "auth_postgres_app_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "auth_postgres_app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "auth_postgres_app_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
