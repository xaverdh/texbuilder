module Main where

import TexBuilder.TexBuilder
import TexBuilder.FileFilters

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
    
    opts = pure texBuilder
      <*> texOpt
      <*> optional pdfOpt
      <*> fileTypesOpt
      -- <*> watchOpt
      <*> noluaFlag
      <*> nolatexmkFlag
      <*> numCompOpt
      <*> extraArgs

pdfOpt = option str
  ( short 'p' <> long "pdf" <> metavar "PDFFILE"
  <> help "The name of the pdf file to write to \
  \ (defaults to TEXFILE with file ending adjusted)." )

texOpt = option str
  ( short 't' <> long "tex" <> metavar "TEXFILE"
  <> help "The main tex file to compile." )

fileTypesOpt = option (maybeReader readExts)
  ( short 'f' <> long "file-types" <> value (Exts ["tex","bib"])
  <> showDefaultWith showExts
  <> help "Watch for changes of all files in the directory,\
          \ with these file endings." )

-- | not yet ready
watchOpt = option str
  ( short 'w' <> long "watch"
  <> help "Watch for changes of these files ONLY." )

noluaFlag = flag LuaLaTex PdfLaTex
  ( long "noluatex"
  <> help "Do not user luaLaTex / use old pdfLaTex instead." )

nolatexmkFlag = flag LatexMk NoLatexMk
  ( long "nolatexmk"
  <> help "Do not go through latexmk, use engine directly." )

numCompOpt = option auto
  ( short 'r' <> long "recompile" <> metavar "NRECOMP" <> value 5
  <> help "The maximum number of times we should attempt to \
          \recompile the document unil it becomes stable \
          \when using luaLaTex / pdfLaTex directly."
  <> showDefault )

extraArgs = many $ strArgument
  ( metavar "EXTRA_ARGS"
  <> help "Extra arguments to pass to the latex engine." )

hdr = "texbuilder: view your latex output pdf while editing"

description =
  "This program allows you to view your document \
  \in mupdf while editing it in your favorite editor. \
  \When you save your document it will recompile it \
  \in a fresh environment, overwrite the output pdf \
  \file and send a signal to mupdf to reload the file. \
  \This effectively allows for a \"continous preview\"-like \
  \experience."


