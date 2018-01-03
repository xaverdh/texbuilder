# Revision history for tex-builder

0.1.4.0
---
* Simplify code by reducing continuation foo somewhat.

0.1.3.0
---
* Add new option to force an initial compile run.
* Enforce threaded runtime in cabal file, fixing problems with e.g. the nix build, which is single threaded by default.

0.1.2.0
---
* Avoid calling external executables by hard coded path,
  improving portability across platforms.

0.1.1.3
---
* Adjust wrong lower bound for base in cabal file.

0.1.1.2
---
* Adjust wrong lower bound for ansi-wl-pprint in cabal file.

0.1.1.1
---
* Changes to README
* Update this changelog

0.1.1.0
---
* Fix bug which caused latexmk not to be used with xelatex.
* Improvements to warnings / error messages.

0.1.0.1
---
* Initial release.
