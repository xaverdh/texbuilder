# texbuilder
## What it is

This program allows you to view your latex document in your pdf
reader while editing it in your favorite editor. When you save
your document, it will recompile it, overwrite the output pdf file
and send a signal to your pdf reader to reload the file.
This effectively allows for a "continuous preview"-like experience.

This will run with *mupdf* and *only on Linux* at the moment.

## Prerequisites and Dependencies
  * linux (we use inotify)
  * a recent cabal / ghc
  * (currently only works with) mupdf
  * working latex with lualatex, xelatex or pdflatex and ideally latexmk
  * make sure to compile this with the -threaded ghc option, otherwise it will not work!

## Build with Cabal from Hackage

```sh
cabal install --bindir . --ghc-option=-threaded texbuilder
```

## How to build from git

```sh
git clone https://gitlab.com/xaverdh/tex-builder
cd tex-builder
cabal sandbox init
cabal install --bindir . --ghc-option=-threaded
```
Alternatively you can use the provided [build script][build-script].
So assuming you have wget you cound do:

```sh
wget -O - https://gitlab.com/xaverdh/tex-builder/raw/master/build | sh
```

## Usage

Say you want to compile a tex file thesis.tex then standard usage would be:
```sh
texbuilder -t thesis.tex
```
By default texbuilder will use a fresh environment for every compile run. This may
sound wasteful, but is quite feasible with average sized tex files on modern hardware.
And it does avoid problems with messed up state, when something goes wrong.

Still if you do have problems with performance, use the ` --stateful ` flag.

Finally if you really really need all those intermediate representation files
in your directory, use ` --persistent `.

## Legal

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

See LICENSE for more details.


[build-script]: https://gitlab.com/xaverdh/tex-builder/blob/master/build

