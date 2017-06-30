module TexBuilder.TexBuilder
  ( texBuilder )
where

import TexBuilder.Engine
import TexBuilder.CompileThread
import TexBuilder.ViewThread

import Control.Monad
import Data.Monoid
import Data.Maybe

import Options.Applicative
import Options.Applicative.Builder
import System.Directory
import System.FilePath
import System.INotify
import System.Exit
import Control.Concurrent
import Control.Concurrent.MVar

texBuilder :: FilePath -> Maybe FilePath -> Engine -> IO ()
texBuilder texfile mbf engine =
  let pdffile = fromMaybe (texfile -<.> "pdf") mbf
   in do
    setupTexFile texfile
    forkOS $ onFileEx pdffile (mupdfView pdffile)
    compileThread texfile pdffile engine

setupTexFile :: FilePath -> IO ()
setupTexFile texfile = do
  ex <- doesFileExist texfile
  unless ex $ do
    putStrLn (texfile <> " does not exist.")
    exitFailure


onFileEx :: FilePath -> IO a -> IO a
onFileEx file action = do
  ex <- doesFileExist file
  case ex of
    True -> action
    False -> do
      inotify <- initINotify
      mvar <- newMVar False
      wdesc <- addWatch inotify
        [Create]
        (takeDirectory file)
        (onCreate mvar)
      takeMVar mvar
      removeWatch wdesc
      action
  where
    onCreate mvar _ = do
      ex <- doesFileExist file
      when ex $ do
        putMVar mvar True



