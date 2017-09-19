module Main where

import TexBuilder.TexBuilder
import TexBuilder.FileFilters

import Data.Semigroup hiding (option)
import Numeric.Natural

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
      <*> depthOpt
      <*> (statefulFlag <|> persistFlag <|> pure Pure)
      -- <*> watchOpt
      <*> noluaFlag
      <*> nolatexmkFlag
      <*> numCompOpt
      <*> extraArgs

pdfOpt :: Parser FilePath
pdfOpt = option str
  ( short 'p' <> long "pdf" <> metavar "PDFFILE"
  <> help "The name of the pdf file to write to \
  \ (defaults to TEXFILE with file ending adjusted)." )

texOpt :: Parser FilePath
texOpt = option str
  ( short 't' <> long "tex" <> metavar "TEXFILE"
  <> help "The main tex file to compile." )

fileTypesOpt :: Parser Exts
fileTypesOpt = option (maybeReader readExts)
  ( short 'f' <> long "file-types" <> value (Exts ["tex","bib"])
  <> showDefaultWith showExts <> metavar "EXTENSIONS"
  <> help "Watch for changes of all files in the directory,\
          \ with these file endings." )

depthOpt :: Parser Natural
depthOpt = option auto
  ( short 'd' <> long "depth" <> value 3
  <> showDefault <> metavar "DEPTH"
  <> help "The depth to descend into directories." )

statefulFlag :: Parser StatePolicy
statefulFlag = flag' Stateful
  ( long "stateful"
  <> help "Run in stateful mode, reusing results from\
          \ previous compile runs. This improves efficiency\
          \ but may lead to artifacts in some situations.")


persistFlag :: Parser StatePolicy
persistFlag = flag' Persistent
  ( long "persistent"
  <> help "Run in persistent mode, using the main directory\
          \ for building. If the state gets messed up, YOU\
          \ will have to fix it.")

-- | not yet ready
watchOpt :: Parser String
watchOpt = option str
  ( short 'w' <> long "watch"
  <> help "Watch for changes of these files ONLY." )

noluaFlag :: Parser UseEngine
noluaFlag = flag UseLuaLaTex UsePdfLaTex
  ( long "noluatex"
  <> help "Do not user luaLaTex / use pdfLaTex instead." )

nolatexmkFlag :: Parser UseLatexMk
nolatexmkFlag = flag UseLatexMk UseNoLatexMk
  ( long "nolatexmk"
  <> help "Do not go through latexmk, use engine directly." )

numCompOpt :: Parser Natural
numCompOpt = option auto
  ( short 'r' <> long "recompile" <> metavar "NRECOMP" <> value 5
  <> help "The maximum number of times we should attempt to \
          \recompile the document unil it becomes stable \
          \when using luaLaTex / pdfLaTex directly."
  <> showDefault )

extraArgs :: Parser [String]
extraArgs = many $ strArgument
  ( metavar "EXTRA_ARGS"
  <> help "Extra arguments to pass to the latex engine." )

hdr = "texbuilder: view your latex output pdf while editing"

description =
  "This program allows you to view your document \
  \in your pdf viewer while editing it in your favorite editor. \
  \When you save your document, it will recompile it, \
  \overwrite the output pdf file and send a signal \
  \to your pdf reader to reload the file. \
  \This effectively allows for a \"continous preview\"-like \
  \experience."


