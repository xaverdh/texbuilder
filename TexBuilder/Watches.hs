{-# LANGUAGE LambdaCase #-}
module TexBuilder.Watches
  ( setupWatches )
where


import TexBuilder.Utils.File

import Control.Monad
import Control.Monad.Extra
import Control.Concurrent.MVar
import System.INotify
import System.Directory
import System.FilePath

import Numeric.Natural

setupWatches :: Natural -- ^ Depth to descend into directories
  -> FilePath -- ^ Path of the directory to watch
  -> (FilePath -> Bool) -- ^ File filter
  -> MVar FilePath
  -> IO b
setupWatches depth texDir fileFilter watchMVar =
  withINotify $ \inotify -> do
    dirMVar <- newEmptyMVar
    walkSubdirs (watch dirMVar inotify) depth texDir
    loop dirMVar inotify
  where
    watch mvar inotify depth dir = 
      void $ addWatch inotify [Modify,Create,Delete] dir
      ( watcherThread mvar depth dir watchMVar fileFilter )

    loop dirMVar inotify = do
      (dir,depth) <- takeMVar dirMVar
      walkSubdirs (watch dirMVar inotify) depth texDir
      loop dirMVar inotify



watcherThread :: MVar (FilePath,Natural)
  -- ^ Directory creation MVar
  -> Natural -- ^ Depth
  -> FilePath -- ^ Directory we are watching
  -> MVar FilePath -- ^ Watch MVar
  -> (FilePath -> Bool) -- ^ File filter
  -> Event -- ^ Recieved event
  -> IO ()
watcherThread dirMVar depth dir watchMVar fileFilter = \case
  Modified False mbPath ->
    whenJust mbPath $ \path ->
      when (fileFilter path) $ post path
  Created False path ->
    when (fileFilter path) $ post path
  Created True path ->
    when (depth > 0) $ putMVar dirMVar (dir </> path,depth) 
  Ignored -> pure ()
  _ -> pure ()
  where
    post path = putMVar watchMVar (dir </> path)


