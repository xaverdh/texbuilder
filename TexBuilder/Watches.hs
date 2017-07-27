{-# language LambdaCase #-}
module TexBuilder.Watches
  ( withWatches )
where


import TexBuilder.Utils

import Control.Monad
import Control.Monad.Extra
import Control.Concurrent.MVar
import System.INotify
import System.Directory

import Numeric.Natural


withWatches :: Natural -- ^ Depth to descend into directories
  -> FilePath -- ^ Path of the directory to watch
  -> (FilePath -> Bool) -- ^ File filter
  -> (MVar FilePath -> IO b) -- ^ Continuation
  -> IO b
withWatches depth texDir fileFilter k =
  withINotify $ \inotify -> do
    wMVar <- newEmptyMVar
    subdirs <- listSubdirs depth texDir
    forM_ subdirs $ watch wMVar inotify
    k wMVar
  where
    watch mvar inotify dir = void $ addWatch inotify
      [Modify,Create,Delete] dir
      ( watcherThread mvar fileFilter )


watcherThread :: MVar FilePath -- ^ Communication MVar
  -> (FilePath -> Bool) -- ^ File filter
  -> Event -- ^ Recieved event
  -> IO ()
watcherThread wMVar fileFilter = \case
  Modified False mbPath ->
    whenJust mbPath $ \path ->
      when (fileFilter path) $ post path
  Created False path ->
    when (fileFilter path) $ post path
  Ignored -> pure ()
    -- ^ TODO: communicate this and other failures to main thread
    --         via MVar FilePath => MVar (Either Err FilePath)
  _ -> pure ()
  where
    post path = makeAbsolute path >>= putMVar wMVar


