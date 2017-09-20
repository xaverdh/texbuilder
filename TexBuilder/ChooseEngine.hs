{-# LANGUAGE LambdaCase #-}
module TexBuilder.ChooseEngine
  ( chooseEngine
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


data UseEngine = UseLuaLaTex | UsePdfLaTex
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


chooseLatexEngine :: UseEngine -> IO UseEngine
chooseLatexEngine = \case
  UseLuaLaTex -> useLua True
  UsePdfLaTex -> usePdf True

chooseLatexMk :: UseLatexMk -> IO UseLatexMk
chooseLatexMk = \case
  UseLatexMk -> useMk
  UseNoLatexMk -> pure UseNoLatexMk


haveLua :: IO Bool
haveLua = haveExe "lualatex"

havePdf :: IO Bool
havePdf = haveExe "pdflatex"

haveMk :: IO Bool
haveMk = haveExe "latexmk"

errorNoEngine = do
  PP.putDoc . PP.red
    $ PP.string "No latex engine found!"
  exitWith $ ExitFailure 2

warn :: String -> IO ()
warn s = PP.putDoc . PP.yellow
  $ PP.string s <> PP.hardline


useMk :: IO UseLatexMk
useMk = ifM haveMk (pure UseLatexMk) $ do
  warn "No latexmk in PATH, using engine directly."
  pure UseNoLatexMk

useLua :: Bool -> IO UseEngine
useLua rec = ifM haveLua (pure UseLuaLaTex) $ do
  warn "No lualatex in PATH."
  if rec then
    warn  "Falling back to pdflatex."
    *> usePdf False
    else errorNoEngine

usePdf :: Bool -> IO UseEngine
usePdf rec = ifM havePdf (pure UsePdfLaTex) $ do
  warn "No pdflatex in PATH."
  if rec then
    warn "Trying lualatex instead."
    *> useLua False
    else errorNoEngine


