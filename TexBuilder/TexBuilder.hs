module TexBuilder.TexBuilder
  ( texBuilder
  , UseEngine(..)
  , UseLatexMk(..) )
where

import TexBuilder.Utils
import TexBuilder.Engine
import TexBuilder.CompileThread
import TexBuilder.ViewThread

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad
import Control.Monad.Extra
import Data.Monoid
import Data.Maybe
import Numeric.Natural

import Options.Applicative
import Options.Applicative.Builder
import System.Directory
import System.FilePath
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
texBuilder texfile mbf useEngine useLatexmk nrecomp extraArgs = do
  issueWarning
  -- ^ Issue warning if appropriate
  assertFileEx texfile
  -- ^ Assert that the tex file exists
  initialCompile pdffile run
  -- ^ Do an initial compile run if appropriate
  sem <- newBinSem
  -- ^ Signaling semaphore connecting the threads
  tid <- forkIO $ compileThread texDir run sem
  -- ^ The thread which compiles the tex code
  onFileEx pdffile (mupdfView pdffile sem)
  -- ^ Enter the main thread which updates the pdf view
  putStrLn "mupdf exited, terminating"
  killThread tid
  where
    texDir = takeDirectory texfile
    
    run = compile engine texfile pdffile extraArgs

    pdffile = fromMaybe (texfile -<.> "pdf") mbf

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
        \things down." <> PP.hardline


initialCompile :: FilePath -> IO PP.Doc -> IO () 
initialCompile pdffile run =
  unlessM (doesFileExist pdffile) $ do
    putStrLn "No ouput file detected, compiling."
    run >>= PP.putDoc


assertFileEx :: FilePath -> IO ()
assertFileEx file =
  unlessM (doesFileExist file) $ do
    putStrLn (file <> " does not exist.")
    exitFailure


