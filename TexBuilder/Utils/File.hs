module TexBuilder.Utils.File where

import Control.Monad
import Control.Monad.Extra

import System.Directory
import System.FilePath
import System.Exit
import System.IO

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Numeric.Natural
import Data.Semigroup
import Data.Maybe


listDirAbsolute :: FilePath -> IO [FilePath]
listDirAbsolute dir = do
  dir' <- makeAbsolute dir
  map (dir'</>) <$> listDirectory dir'


listSubdirs :: Natural -> FilePath -> IO [FilePath]
listSubdirs = walkSubdirs (const pure)


walkSubdirs :: (Natural -> FilePath -> IO a)
  -> Natural -> FilePath
  -> IO [a]
walkSubdirs f 0 dir = fmap pure $ f 0 dir
walkSubdirs f depth dir = do
  paths <- listDirAbsolute dir
  dirs <- filterM doesDirectoryExist paths
  results <- forM dirs $ walkSubdirs f (depth-1)
  (:join results) <$> f depth dir


searchFilesWith :: (FilePath -> Bool) -> [FilePath] -> IO [FilePath]
searchFilesWith f dirs = do
  paths <- join <$> forM dirs listDirAbsolute
  pure $ filter f paths


-- | Copy file /file/ from dir1 to dir2, creating
--   directories as needed. The path of the resulting
--   copy relative to dir2 will be the same as the path
--   of the original file relative dir1
copyRelative :: FilePath -> FilePath -> FilePath -> IO ()
copyRelative dir1 dir2 file = do
  dir1' <- makeAbsolute dir1
  let relFile = makeRelative dir1' file
  let destFile = dir2 </> relFile
  let destDir = takeDirectory destFile
  createDirectoryIfMissing True destDir
  copyFile file destFile

assertFileEx :: FilePath -> IO ()
assertFileEx file =
  unlessM (doesFileExist file) $ do
    PP.hPutDoc stderr . PP.red
      $ PP.string (file <> " does not exist.")
      <> PP.hardline
    exitFailure

haveExe :: String -> IO Bool
haveExe name = isJust <$> findExecutable name


