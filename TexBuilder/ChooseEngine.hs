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
chooseEngine useEngine useLatexmk =
  case (useEngine,useLatexmk) of
    (UseLuaLaTex,UseLatexMk) -> useLuaMk True
    (UseLuaLaTex,UseNoLatexMk) -> useLua True
    (UsePdfLaTex,UseLatexMk) -> usePdfMk True
    (UsePdfLaTex,UseNoLatexMk) -> usePdf True

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


useLuaMk :: Bool -> IO Engine
useLuaMk rec = haveLua >>= \case
  False -> do
    warn "No lualatex in PATH."
    if rec then
      warn "Falling back to pdflatex."
      *> usePdfMk False
      else errorNoEngine
  True -> ifM haveMk (pure LuaLaTexMk) $ do
    warn "No latexmk in PATH, using lualatex directly."
    pure LuaLaTex

useLua :: Bool -> IO Engine
useLua rec = ifM haveLua (pure LuaLaTex) $ do
  warn "No lualatex in PATH."
  if rec then
    warn  "Falling back to pdflatex."
    *> usePdf False
    else errorNoEngine

usePdf :: Bool -> IO Engine
usePdf rec = ifM havePdf (pure PdfLaTex) $ do
  warn "No pdflatex in PATH."
  if rec then
    warn "Trying lualatex instead."
    *> useLua False
    else errorNoEngine

usePdfMk :: Bool -> IO Engine
usePdfMk rec = havePdf >>= \case
  False -> do
    warn "No pdflatex in PATH."
    if rec then
      warn "Trying lualatex instead."
      *> useLuaMk False
      else errorNoEngine
  True -> ifM haveMk (pure PdfLaTexMk) $ do
    warn "No latexmk in PATH, using pdflatex directly."
    pure PdfLaTex


