# texbuilder
## What it is

This program allows you to view your document in your pdf reader
while editing it in your favorite editor. When you save your document
it will recompile it, overwrite the output pdf file and send
a signal to your pdf reader to reload the file.
This effectively allows for a "continous preview"-like experience.

## Prerequisites and Dependencies
  * linux (we use inotify extensively)
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
Alternatively you can use the provided [build script][build-script].
So assuming you have wget you cound do:
wget -O - https://gitlab.com/xaverdh/tex-builder/raw/master/build | sh

## Usage

Say you want to compile a tex file thesis.tex then standard usage would be:
```sh
texbuilder -t thesis.tex
```
By default texbuilder will use a fresh environment for every compile run. This may
sound wasteful, but is quite feasible with average sized tex files on modern harware.
And it does avoid problems with messed up state, when something goes wrong.

Still if you do have problems with performance, use the ` --stateful ` flag.

Finally if you really really need all those intermediate representation files
in your directory, use ` --persistent `.


[build-script]: https://gitlab.com/xaverdh/tex-builder/blob/master/build

