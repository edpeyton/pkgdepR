## Resubmission
This is a resubmission. In this version I have:

* Edited the DESCRIPTION file title to remove "This package" reference.

* Added `\value{...}` entries to the Rd files for
  * print.pkgdepR
  * summary.pkgdepR
  
* Added further details about the object class `pkgdepR` to
  * `pkgdepR-package.Rd`
  * `deps.Rd`

* Have re-run all the same R-CMD-check's as done in initial submission
  * There were no ERRORs, WARNINGs or NOTEs.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

Package has been deployed on macOS, Windows and Ubuntu via GitHub Actions:
(https://github.com/edpeyton/pkgdepR/actions).

* macOS-latest (release)
* windows-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (release)
* ubuntu-latest (oldrel-1)

Also tested on (`devtools::check_win_devel`)

* windows (devel)

pkgdown site available:
https://pkgdepr.org/.
