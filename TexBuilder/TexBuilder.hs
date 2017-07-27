{-# language PackageImports #-}
module TexBuilder.TexBuilder
  ( texBuilder
  , StatePolicy(..)
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


data UseEngine = LuaLaTex | PdfLaTex
data UseLatexMk = LatexMk | NoLatexMk
data StatePolicy = Pure | Stateful | Persistent

texBuilder :: FilePath
  -> Maybe FilePath
  -> Exts
  -> Natural
  -> StatePolicy
  -> UseEngine
  -> UseLatexMk
  -> Natural
  -> [String]
  -> IO ()
texBuilder 
  texfile mbPdfFile exts depth statePolicy
  useEngine useLatexmk nrecomp extraArgs =
  withModRunAction statePolicy texDir listSrcFiles runRaw
    $ \run -> do
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
    
    runRaw = compile engine texfile pdffile extraArgs

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


withDirSetup :: FilePath
  -> FilePath
  -> IO [FilePath]
  -> ( FilePath -> IO a ) -> IO a
withDirSetup wdir texDir listSrc k = do
  files <- listSrc
  forM_ files $ copyRelative texDir wdir
  k wdir

withModRunAction :: StatePolicy
  -> FilePath
  -> IO [FilePath]
  -> ( FilePath -> IO PP.Doc )
  -> ( IO PP.Doc -> IO a ) -> IO a
withModRunAction statePolicy texDir listSrc run k =
  case statePolicy of
    Pure -> k $ withTmp $ \tmpdir -> runIn tmpdir
    -- ^ Create fresh temporary directory every time
    Stateful-> withTmp $ \tmpdir -> k $ runIn tmpdir
    -- ^ Run in stateful temporary directory
    Persistent -> getCurrentDirectory >>= \wdir -> k $ runIn wdir
    -- ^ Run in persistent working directory
  where
    withTmp = withSystemTempDirectory "texbuilder"
    runIn wdir = withDirSetup wdir texDir listSrc run


assertFileEx :: FilePath -> IO ()
assertFileEx file =
  unlessM (doesFileExist file) $ do
    putStrLn (file <> " does not exist.")
    exitFailure


