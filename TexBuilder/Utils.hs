module TexBuilder.Utils where


import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans

import System.Directory
import System.FilePath

import Data.Monoid
import Numeric.Natural


listDirAbsolute :: FilePath -> IO [FilePath]
listDirAbsolute dir = do
  dir' <- makeAbsolute dir
  map (dir'</>) <$> listDirectory dir'

listSubdirs :: Natural -> FilePath -> IO [FilePath]
listSubdirs 0 dir = pure [dir]
listSubdirs depth dir = do
  paths <- listDirAbsolute dir
  dirs <- filterM doesDirectoryExist paths
  subdirs <- forM dirs $ listSubdirs (depth-1)
  pure $ dir : join subdirs

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



