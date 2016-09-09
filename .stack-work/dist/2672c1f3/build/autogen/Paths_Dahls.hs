module Paths_Dahls (
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

bindir     = "C:\\Users\\Joshua\\Desktop\\Dahls\\.stack-work\\install\\29d018e7\\bin"
libdir     = "C:\\Users\\Joshua\\Desktop\\Dahls\\.stack-work\\install\\29d018e7\\lib\\x86_64-windows-ghc-7.10.3\\Dahls-0.1.0.0-J5jS2Hw2NbS5Ghx6XLuP1v"
datadir    = "C:\\Users\\Joshua\\Desktop\\Dahls\\.stack-work\\install\\29d018e7\\share\\x86_64-windows-ghc-7.10.3\\Dahls-0.1.0.0"
libexecdir = "C:\\Users\\Joshua\\Desktop\\Dahls\\.stack-work\\install\\29d018e7\\libexec"
sysconfdir = "C:\\Users\\Joshua\\Desktop\\Dahls\\.stack-work\\install\\29d018e7\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Dahls_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Dahls_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Dahls_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Dahls_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Dahls_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
