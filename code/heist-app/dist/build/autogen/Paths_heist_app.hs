module Paths_heist_app (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/heist-app/heist-app/.hsenv/cabal/bin"
libdir     = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/heist-app/heist-app/.hsenv/cabal/lib/heist-app-0.1/ghc-7.6.3"
datadir    = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/heist-app/heist-app/.hsenv/cabal/share/heist-app-0.1"
libexecdir = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/heist-app/heist-app/.hsenv/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "heist_app_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "heist_app_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "heist_app_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "heist_app_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)