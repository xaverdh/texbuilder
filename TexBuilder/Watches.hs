{-# language LambdaCase #-}
module TexBuilder.Watches
  ( withWatches )
where


import Control.Monad
import Control.Monad.Extra
import Control.Concurrent.MVar
import System.INotify


withWatches :: FilePath -- ^ Path of the directory to watch
  -> (FilePath -> Bool) -- File filter
  -> (MVar FilePath -> IO b) -> IO b
withWatches dir fileFilter k =
  withINotify $ \inotify -> do
    let watch = void . addWatch inotify [Modify,Create] dir
    wMVar <- newEmptyMVar
    watch (watcherThread wMVar watch fileFilter)
    k wMVar


watcherThread :: MVar FilePath
  -> ((Event -> IO ()) -> IO ())
  -> (FilePath -> Bool) -- File filter
  -> Event
  -> IO ()
watcherThread wMVar watch fileFilter = go
  where
    go = \case
      Modified False mbPath ->
        whenJust mbPath $ \path ->
          when (fileFilter path) $ putMVar wMVar path
      Created False path ->
        when (fileFilter path) $ putMVar wMVar path
      Ignored -> watch go
      _ -> pure ()


