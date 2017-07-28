module TexBuilder.Utils where

import TexBuilder.Utils.BinSem

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans
import Control.DeepSeq

import System.INotify
import System.Directory
import System.FilePath

-- import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
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


-- Be careful with this stuff.
-- Inotify is _inode_ based, but these functions want
-- to watch _paths_. So they reregister the watches
-- when necessary. Still it is probably best to look
-- at the implementation before using them.
-- Also this whole business is rather error prone..

-- ^ Simple version of /withEvent/
onEvent :: [EventVariety] -> FilePath -> IO a -> IO ()
onEvent variety file k = withEvent variety file (const k)

-- ^ Execute action once an event involving the given
--   filepath happens.
--   This function blocks.
withEvent :: [EventVariety]
  -> FilePath
  -> (Event -> IO a)
  -> IO ()
withEvent variety file k =
  withINotify $ \inotify -> do
    sem <- newBinSem
    wd <- addWatch inotify variety file
      (k >=> const (trySignal' sem))
    wait sem
    removeWatch wd
  
-- ^ Simple version of /loopWithEvent/
loopOnEvent :: [EventVariety] -> FilePath -> IO Bool -> IO ()
loopOnEvent variety file k = loopWithEvent variety file (const k)

-- ^ Execute action every time an Event involving the given
--   filepath happens untils the callback returns True.
--   This function blocks.
loopWithEvent :: [EventVariety] -- ^ Events to watch
  -> FilePath -- ^ Filepath to watch
  -> (Event -> IO Bool)
  -- ^ Callback. If it returns True we stop looping
  -> IO ()
loopWithEvent variety file k = 
  withINotify $ \inotify -> do
    doneMVar <- newEmptyMVar
    loopWithEventImpl variety file k inotify doneMVar

loopWithEventImpl :: [EventVariety]
  -> FilePath
  -> (Event -> IO Bool)
  -> INotify
  -> MVar Bool
  -> IO ()
loopWithEventImpl variety file k inotify doneMVar = loop
  where
    loop = do
      wd <- watch
      done <- takeMVar doneMVar
      removeWatch wd
      unless done loop
    watch = addWatch inotify variety file
            (k >=> putMVar doneMVar)

waitForFile :: FilePath -> IO ()
waitForFile file = 
  loopOnEvent [Create] dir (doesFileExist file)
  where dir = takeDirectory file

onFileEx :: FilePath -> IO a -> IO a
onFileEx file action =
  unlessM
    (doesFileExist file)
    (waitForFile file)
  >> action



