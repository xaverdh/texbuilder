# texbuilder
## What it is

This program allows you to view your document in mupdf while
editing it in your favorite editor. When you save your document
it will recompile it in a fresh environment, overwrite the output
pdf file and send a signal to mupdf to reload the file.
This effectively allows for a "continous preview"-like experience.

## Prerequisites and Dependencies
  * A recent cabal / ghc
  * (currently only works with) mupdf
  * working latex with either lualatex or pdflatex

## How to build

```sh
git clone https://gitlab.com/xaverdh/tex-builder
cd tex-builder
cabal sandbox init
cabal install --bindir . --ghc-option=-threaded
```
Alternatively you can user the provided [build script][build-script].
So assuming you have wget you cound do:
wget -O - https://gitlab.com/xaverdh/tex-builder/raw/master/build | sh





[build-script]: https://gitlab.com/xaverdh/tex-builder/blob/master/build

