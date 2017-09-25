{-# LANGUAGE LambdaCase #-}
module TexBuilder.ChooseEngine
  ( chooseEngine
  , readUseEngine
  , showUseEngine
  , UseEngine(..)
  , UseLatexMk(..) )
where

import TexBuilder.Utils.File
import TexBuilder.Engine

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import System.Exit
import Data.Monoid



data UseEngine = 
  UseLuaLaTex
  | UsePdfLaTex
  | UseXeLaTex
  deriving (Enum,Bounded)

readUseEngine :: String -> Maybe UseEngine
readUseEngine s
  | s == "lualatex" = Just UseLuaLaTex
  | s == "xelatex"  = Just UsePdfLaTex
  | s == "pdflatex" = Just UseXeLaTex
  | otherwise  = Nothing

showUseEngine :: UseEngine -> String
showUseEngine = enginePath

data UseLatexMk = UseLatexMk | UseNoLatexMk


chooseEngine :: UseEngine -> UseLatexMk -> IO Engine
chooseEngine useEngine useLatexmk = do
  engine <- chooseLatexEngine useEngine
  texmk <- chooseLatexMk useLatexmk
  pure $ case (engine,texmk) of
    (UseLuaLaTex,UseLatexMk) -> LuaLaTexMk
    (UseLuaLaTex,UseNoLatexMk) -> LuaLaTex
    (UsePdfLaTex,UseLatexMk) -> PdfLaTexMk
    (UsePdfLaTex,UseNoLatexMk) -> PdfLaTex
    (UseXeLaTex,UseLatexMk) -> XeLaTexMk
    (UseXeLaTex,UseNoLatexMk) -> XeLaTex


chooseLatexEngine :: UseEngine -> IO UseEngine
chooseLatexEngine = fallback . \case
  UseLuaLaTex -> UseLuaLaTex : [UsePdfLaTex,UseXeLaTex]
  UsePdfLaTex -> UsePdfLaTex : [UseXeLaTex,UseLuaLaTex]
  UseXeLaTex -> UseXeLaTex : [UsePdfLaTex,UseLuaLaTex]
  where
    numEngines = fromEnum ( maxBound :: UseEngine )

chooseLatexMk :: UseLatexMk -> IO UseLatexMk
chooseLatexMk = \case
  UseLatexMk -> useMk
  UseNoLatexMk -> pure UseNoLatexMk

haveMk :: IO Bool
haveMk = haveExe "latexmk"

useMk :: IO UseLatexMk
useMk = ifM haveMk (pure UseLatexMk) $ do
  warn "No latexmk in PATH, using engine directly."
  pure UseNoLatexMk


enginePath :: UseEngine -> FilePath
enginePath = \case
  UseLuaLaTex -> "lualatex"
  UsePdfLaTex -> "pdflatex"
  UseXeLaTex -> "xelatex"

haveEngine :: UseEngine -> IO Bool
haveEngine = haveExe . enginePath

fallback :: [UseEngine] -> IO UseEngine
fallback = \case
  [] -> errorNoEngine
  useEng:useEngines ->
    ifM (haveEngine useEng) (pure useEng) $ do
      warn (enginePath useEng <> " not in PATH.")
      warnFallback useEngines
      fallback useEngines
  where
    warnFallback [] = pure ()
    warnFallback (useEng:_) = 
      warn ("Falling back to " <> enginePath useEng <> ".")

warn :: String -> IO ()
warn s = PP.putDoc . PP.yellow
  $ PP.string s <> PP.hardline

errorNoEngine = do
  PP.putDoc . PP.red
    $ PP.string "No latex engine found!"
  exitWith $ ExitFailure 2


