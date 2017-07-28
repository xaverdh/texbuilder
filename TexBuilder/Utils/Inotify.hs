module TexBuilder.Utils.Inotify where

import TexBuilder.Utils.BinSem

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Extra

import System.INotify
import System.Directory
import System.FilePath


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





