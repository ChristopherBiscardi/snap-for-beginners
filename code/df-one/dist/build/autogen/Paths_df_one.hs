module Paths_df_one (
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

bindir     = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/05-digestive-functors/df-one/.hsenv/cabal/bin"
libdir     = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/05-digestive-functors/df-one/.hsenv/cabal/lib/df-one-0.1/ghc-7.6.3"
datadir    = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/05-digestive-functors/df-one/.hsenv/cabal/share/df-one-0.1"
libexecdir = "/Users/chris/Dropbox/publishing/snap-for-beginners/code/05-digestive-functors/df-one/.hsenv/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "df_one_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "df_one_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "df_one_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "df_one_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
