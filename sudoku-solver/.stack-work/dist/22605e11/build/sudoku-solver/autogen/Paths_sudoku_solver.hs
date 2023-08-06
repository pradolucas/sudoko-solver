{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_sudoku_solver (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\fernanda.sayuri.ito\\OneDrive - Accenture\\Desktop\\sudoku-solver\\.stack-work\\install\\7b252857\\bin"
libdir     = "C:\\Users\\fernanda.sayuri.ito\\OneDrive - Accenture\\Desktop\\sudoku-solver\\.stack-work\\install\\7b252857\\lib\\x86_64-windows-ghc-9.4.5\\sudoku-solver-0.1.0.0-7OVWrh60hDXDtVMwWh2irn-sudoku-solver"
dynlibdir  = "C:\\Users\\fernanda.sayuri.ito\\OneDrive - Accenture\\Desktop\\sudoku-solver\\.stack-work\\install\\7b252857\\lib\\x86_64-windows-ghc-9.4.5"
datadir    = "C:\\Users\\fernanda.sayuri.ito\\OneDrive - Accenture\\Desktop\\sudoku-solver\\.stack-work\\install\\7b252857\\share\\x86_64-windows-ghc-9.4.5\\sudoku-solver-0.1.0.0"
libexecdir = "C:\\Users\\fernanda.sayuri.ito\\OneDrive - Accenture\\Desktop\\sudoku-solver\\.stack-work\\install\\7b252857\\libexec\\x86_64-windows-ghc-9.4.5\\sudoku-solver-0.1.0.0"
sysconfdir = "C:\\Users\\fernanda.sayuri.ito\\OneDrive - Accenture\\Desktop\\sudoku-solver\\.stack-work\\install\\7b252857\\etc"

getBinDir     = catchIO (getEnv "sudoku_solver_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "sudoku_solver_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "sudoku_solver_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "sudoku_solver_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_solver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_solver_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'