{-# language PackageImports #-}
module TexBuilder.TexBuilder
  ( texBuilder
  , UseEngine(..)
  , UseLatexMk(..) )
where

import TexBuilder.Utils
import TexBuilder.Engine
import TexBuilder.Watches
import TexBuilder.CompileThread
import TexBuilder.ViewThread
import TexBuilder.FileFilters

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad
import Control.Monad.Extra
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Numeric.Natural
import "cryptonite" Crypto.Hash

import Options.Applicative
import Options.Applicative.Builder
import System.Directory
import System.FilePath
import System.Exit
import System.IO.Temp
import Control.Concurrent
import Control.Concurrent.MVar

import Debug.Trace

data UseEngine = LuaLaTex | PdfLaTex
data UseLatexMk = LatexMk | NoLatexMk

texBuilder :: FilePath
  -> Maybe FilePath
  -> Exts
  -> Natural
  -> UseEngine
  -> UseLatexMk
  -> Natural
  -> [String]
  -> IO ()
texBuilder 
  texfile mbPdfFile exts depth useEngine useLatexmk
  nrecomp extraArgs = do
  issueWarning
  -- ^ Issue warning if appropriate
  assertFileEx texfile
  -- ^ Assert that the tex file exists
  initialCompile pdffile run
  -- ^ Do an initial compile run if appropriate
  sem <- newBinSem
  -- ^ Signaling semaphore connecting the threads
  tid <- forkIO $ 
    withInitialHashes listSrcFiles $ \hashes ->
      withWatches depth texDir fileFilter $ \wMVar ->
        compileThread run sem wMVar hashes
  -- ^ The thread which compiles the tex code
  onFileEx pdffile ( mupdfView pdffile sem )
  -- ^ Enter the main thread which updates the pdf view
  putStrLn "mupdf exited, terminating"
  killThread tid
  where
    fileFilter = extFilter exts

    texDir = takeDirectory texfile
    
    listSrcFiles = listSourceFiles depth texDir fileFilter
    
    run = -- withTmpDirSetup texDir listSrcFiles $
      withStatefulTmpDirSetup "/tmp/texbuilder-debug" texDir listSrcFiles $
      compile engine texfile pdffile extraArgs

    pdffile = fromMaybe (texfile -<.> "pdf") mbPdfFile

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

listSourceFiles :: Natural
  -> FilePath
  -> ( FilePath -> Bool )
  -> IO [FilePath]
listSourceFiles depth texDir fileFilter = 
  listSubdirs depth texDir
  >>= searchFilesWith fileFilter

withInitialHashes :: IO [FilePath]
  -> ( M.Map FilePath (Digest MD5) -> IO b)
  -> IO b
withInitialHashes listSrc k = do
  files <- listSrc
  withHashes files $ k . M.fromList . zip files


copyRelative :: FilePath -> FilePath -> FilePath -> IO ()
copyRelative dir1 dir2 file = do
  createDirectoryIfMissing True destDir
  traceShowId file
  traceShowId destFile
  copyFile file destFile
  where
    relFile = makeRelative dir1 file
    destFile = dir2 </> relFile
    destDir = takeDirectory destFile


withTmpDirSetup :: FilePath
  -> IO [FilePath]
  -> ( FilePath -> IO a ) -> IO a
withTmpDirSetup texDir listSrc k =
  withSystemTempDirectory "texbuilder" $ \tmpdir -> do
    files <- listSrc
    forM_ files $ copyRelative texDir tmpdir
    k tmpdir

withStatefulTmpDirSetup :: FilePath
  -> FilePath
  -> IO [FilePath]
  -> ( FilePath -> IO a ) -> IO a
withStatefulTmpDirSetup tmpdir texDir listSrc k = do
  files <- listSrc
  forM_ files $ copyRelative texDir tmpdir
  k tmpdir


assertFileEx :: FilePath -> IO ()
assertFileEx file =
  unlessM (doesFileExist file) $ do
    putStrLn (file <> " does not exist.")
    exitFailure


