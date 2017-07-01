module TexBuilder.TexBuilder
  ( texBuilder
  , UseEngine(..)
  , UseLatexMk(..) )
where

import TexBuilder.Engine
import TexBuilder.CompileThread
import TexBuilder.ViewThread

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad
import Data.Monoid
import Data.Maybe
import Numeric.Natural

import Options.Applicative
import Options.Applicative.Builder
import System.Directory
import System.FilePath
import System.INotify
import System.Exit
import Control.Concurrent
import Control.Concurrent.MVar

data UseEngine = LuaLaTex | PdfLaTex
data UseLatexMk = LatexMk | NoLatexMk

texBuilder :: FilePath
  -> Maybe FilePath
  -> UseEngine
  -> UseLatexMk
  -> Natural
  -> [String]
  -> IO ()
texBuilder texfile mbf useEngine useLatexmk nrecomp extraArgs =
  let pdffile = fromMaybe (texfile -<.> "pdf") mbf
   in do
    issueWarning
    setupTexFile texfile
    mvar <- newEmptyMVar
    forkIO $ do
      onFileEx pdffile (mupdfView pdffile)
      putMVar mvar ()
    tid <- forkIO $ compileThread texfile pdffile engine extraArgs
    takeMVar mvar
    putStrLn "mupdf exited, terminating"
    killThread tid
  where
    engine = case (useEngine,useLatexmk) of
      (LuaLaTex,LatexMk) -> luaLaTexMk
      (LuaLaTex,NoLatexMk) -> recompile nrecomp luaLaTex
      (PdfLaTex,LatexMk) -> pdfLaTexMk
      (PdfLaTex,NoLatexMk) -> recompile nrecomp pdfLaTex
    
    issueWarning = case (useEngine,useLatexmk) of
      (LuaLaTex,NoLatexMk) -> PP.putDoc warning
      _ -> pure ()
    
    warning = PP.yellow $ PP.text
        "Note that lualatex does not currently respect \
        \SOURCE_DATE_EPOCH, so the source will often be \
        \rebuild the maximum number of times, slowing \
        \things down."


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
      mvar <- newEmptyMVar
      wdesc <- addWatch inotify
        [Create] dir
        (onCreate mvar)
      takeMVar mvar
      removeWatch wdesc
      action
  where
    dir = takeDirectory file
    onCreate mvar _ = do
      ex <- doesFileExist file
      when ex $ putMVar mvar ()



