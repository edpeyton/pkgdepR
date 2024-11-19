
## Resubmssion with patch
* Fixing bug not discovered in initial push to CRAN.
* Fixed some roxygen2 documentation causing NOTEs in CRAN build.

## Resubmssion
Fixing problems with CRAN submission:
* Fixing problem in roxygen2
* Fixing CITATION file

## Package update
Minor package update mainly addressing:

* Fixing bug in plot method.
* Fixing issue with identifying function links for non-exported functions.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

Package has been deployed on macOS, Windows and Ubuntu via GitHub Actions:
(https://github.com/edpeyton/pkgdepR/actions).

* macOS-latest (release)
* windows-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (release)
* ubuntu-latest (oldrel-1)

## revdepcheck

No issues after running `revdepcheck::revdep_check(...)`.


`pkgdown` site available at:
https://pkgdepr.org/.
