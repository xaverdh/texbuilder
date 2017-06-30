module Main where

import TexBuilder.Engine
import TexBuilder.TexBuilder

import Data.Monoid

import Options.Applicative
import Options.Applicative.Builder


main :: IO ()
main = execParser parser >>= id
  where
    parser = info (helper <*> opts)
      (fullDesc
        <> header hdr
        <> progDesc description )
    
    opts = texBuilder
      <$> texOpt
      <*> optional pdfOpt
      <*> noluaFlag

pdfOpt = option str
  ( short 'p' <> long "pdf" <> metavar "PDFFILE"
  <> help "The name of the pdf file to write to \
  \ (defaults to TEXFILE with file ending adjusted)" )

texOpt = option str
  ( short 't' <> long "tex" <> metavar "TEXFILE"
  <> help "The tex file to watch" )

noluaFlag = flag lualatex pdflatex
  ( long "noluatex"
  <> help "no not user lualatex / use old pdflatex instead" )

hdr = "texbuilder: view your latex output pdf while editing"

description =
  "This program allows you to view your document \
  \in mupdf while editing it in your favorite editor. \
  \When you save your document it will recompile it \
  \in a fresh environment, overwrite the output pdf \
  \file and send a signal to mupdf to reload the file. \
  \This effectively allows for a \"continous preview\"-like \
  \experience."


