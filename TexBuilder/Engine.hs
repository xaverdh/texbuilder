{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module TexBuilder.Engine
  ( Engine(..)
  , compile
  , luaLaTex
  , pdfLaTex
  , recompile
  , luaLaTexMk
  , pdfLaTexMk )
where

import TexBuilder.Utils.Hashing
import TexBuilder.Utils.File

-- import Data.Semigroup
import Data.Monoid

import Numeric.Natural
import Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad.State

import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.Environment (setEnv)
import System.Posix.Time (epochTime)

data Engine =
  LuaLaTexMk
  | LuaLaTex
  | PdfLaTexMk
  | PdfLaTex
  | XeLaTexMk
  | XeLaTex

runEngine :: Natural -> Engine -> EngineImpl
runEngine nrecomp = \case
  LuaLaTexMk -> luaLaTexMk
  LuaLaTex -> recompile nrecomp luaLaTex
  PdfLaTexMk -> pdfLaTexMk
  PdfLaTex -> recompile nrecomp pdfLaTex
  XeLaTexMk -> xeLaTexMk
  XeLaTex -> recompile nrecomp xeLaTex

type EngineImpl =
  FilePath -> FilePath -> [String] -> IO (Either String FilePath)

data RecompileState =
  StInitial Natural
  | StSucc Natural FilePath (Digest MD5)


-- | Recompile the code until the output stabilizes
--   or maxNum compile runs is reached.
--   Care must be taken to make the latex output reproductible.
--   Otherwise the output will never stablilize.
recompile :: Natural -> EngineImpl -> EngineImpl
recompile maxNum engine outDir texfile extraArgs = do
  time <- show <$> epochTime
  evalStateT (recompileSt (run time)) (StInitial maxNum)
  where
    run time = do
      setEnv "SOURCE_DATE_EPOCH" time
      -- ^ For reproductible output
      engine outDir texfile extraArgs


recompileSt :: IO (Either String FilePath)
  -> StateT RecompileState IO (Either String FilePath)
recompileSt run = get >>= \case
  StInitial maxNum -> go $ succ maxNum
  StSucc i path oldHash ->
    if i <= 0 then done path
      else go $ \path hash ->
        if hash == oldHash then done path
          else succ i path hash
  where
    go k = lift run >>= \case
      Left err -> failed err
      Right path -> withHash path $ k path
    
    succ i path hash = do
      put $ StSucc (i-1) path hash
      recompileSt run
    
    done = pure . Right
    failed = pure . Left


luaLaTex :: EngineImpl
luaLaTex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/lualatex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "--interaction=scrollmode"
        , "--output-directory=" <> outDir
        , "--jobname=" <> jobname
        , "--output-format=pdf"
        , "--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

pdfLaTex :: EngineImpl
pdfLaTex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/pdflatex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args = 
        [ "--interaction=scrollmode"
        , "--output-directory=" <> outDir
        , "--jobname=" <> jobname
        , "--output-format=pdf"
        , "--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

xeLaTex :: EngineImpl
xeLaTex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/xelatex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "--interaction=scrollmode"
        , "--output-directory=" <> outDir
        , "--jobname=" <> jobname
        , "--output-format=pdf"
        , "--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"


xeLaTexMk :: EngineImpl
xeLaTexMk outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-xelatex"
        , "-output-directory=" <> outDir
        , "-jobname=" <> jobname ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"



luaLaTexMk :: EngineImpl
luaLaTexMk outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-lualatex"
        , "-output-directory=" <> outDir
        , "-jobname=" <> jobname ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

pdfLaTexMk :: EngineImpl
pdfLaTexMk outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-pdf"
        , "-output-directory=" <> outDir
        , "-jobname=" <> jobname ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

-- | Compile the tex code and (if successful)
--   write the output to given path. Returns human readable
--   information about the build success / failure.
compile :: Engine
  -> Natural
  -> FilePath
  -> FilePath
  -> [String]
  -> FilePath
  -> IO PP.Doc
compile engine nrecomp texfile pdffile extraArgs dir = do
  initT <- epochTime
  runEngine nrecomp engine dir texfile extraArgs >>= \case
    Left err -> pure $ PP.red $ PP.text err
    Right outFile -> do
      finalT <- epochTime
      copyFile outFile pdffile
      pure $ PP.green $
        "Successful build from"
        <+> PP.text dir <> ","
        <+> "build time was"
        <+> (PP.text . show $ finalT - initT)
        <+> "s" <> PP.hardline

  




