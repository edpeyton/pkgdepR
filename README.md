
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgdepR

[![](https://cranlogs.r-pkg.org/badges/pkgdepR)](https://cran.r-project.org/package=pkgdepR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/pkgdepR)](https://cran.r-project.org/package=pkgdepR)
[![R build
status](https://github.com/edpeyton/pkgdepR/workflows/R-CMD-check/badge.svg)](https://github.com/edpeyton/pkgdepR/actions/)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgdepR)](https://CRAN.R-project.org/package=pkgdepR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

# Introduction

Have you ever wanted to visualize dependencies between functions for a
group of R packages you have developed?

Then try using `{pkgdepR}`!

`{pkgdepR}` takes any number of (correctly compiled) R packages and
finds the links between all of the function in those namespaces.

## About

### Why create `{pkgdepR}`?

`{pkgdepR}` was created to solve a particular problem I had faced when
developing interrelated R packages within organizations. Oftentimes, I
would want to see (visually) how all the functions in one package
interacted with all the functions in another package.

This was particularly useful in managing the function dependencies
across a large code base of R packages.

### How does `{pkgdepR}` work?

`{pkgdepR}` simply takes as an argument a vector of package names (that
should already be on the search path) and explores how each of the
functions in each of the namespaces interact. It does this in two
stages:

-   Getting all intra-package function dependencies for each package;
    *and*  
-   Getting all inter-package function dependencies for each combination
    of packages.

Each defined name in a particular package’s namespace that is also a
function is then decomposed. From this decomposition, if any function is
found to be called within its contents, then a link is created. To
properly identify distinct name calls, a search is done for a package
tag preceding the name (i.e. `::`). If no tag exists, then a search for
an imported function is conducted. On the other hand, if the function
name is already declared in the primary namespace, then it is obviously
that function being called.

If a function is ambiguously called (without a package tag or not being
explicitly imported from another namespace) then it is deliberately
ignored in the linkage. This is because the linkage would be
environment-dependent and would change depending on the contents of the
search path in that particular session.

### S3 methods for `{pkgdepR}` objects

The main wrapper function for `{pkgdepR}` is `pkgdepR::deps(...)` which
returns an object of class `pkgdepR`. An S3 method has been created for
objects of class `pkgdepR` so they can be easily plotted. See
`?pkgdepR::plot.pkgdepR` for further details.

`{pkgdepR}` makes use of the fantastic
[visNetwork](https://github.com/datastorm-open/visNetwork) package for
creating an interactive network visualization of the functions present
in R packages.

All implemented methods for this class are:

``` r
methods(class = "pkgdepR")
#> [1] plot    print   summary
#> see '?methods' for accessing help and source code
```

## Caveats

The links between functions are determined **statically**, meaning the
dependencies are identified without executing any code. As such, any
functions that are defined dynamically at run-time will not be picked
up. In the example below, `foo()` will be picked up as it is declared in
the package’s namespace. However, `bar()` will not be picked up as it is
only defined at run-time when `foo()` is called.

``` r
# @title foo function
# @export
foo = function() { # - this will be picked up!

  bar = function() { # - this won't be picked up!
  
  }
  
  bar()

}
```

Similarly, there can be cases where linkages are created that may not
occur at run-time. Let’s add to the previous example, by adding a
function `bar()` declared in the package namespace we are interested in.

``` r
# @title bar function
# @export
bar = function() { # - this will be picked up!

}
```

Then, statically, it appears that `foo()` is calling the `bar()` from
the namespace we are interested in. However, at run-time, we know this
is **not** the case, as `bar()` is clearly defined locally and it is the
local function that is called. In this instance, `pkgdepR` will show a
link between `package::foo()` and `package::bar()` when no real run-time
dependency exists.

These cases will rarely arise in normal package development, but it is
important to be aware of the behaviour nonetheless.

## Installation

You can install the released version of `{pkgdepR}` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pkgdepR")
```

And the development version from
[GitHub](https://github.com/edpeyton/pkgdepR) with:

``` r
devtools::install_github("edpeyton/pkgdepR")
```

# How to use

Here we’ll show an example of how to use `{pkgdepR}`.

First, let’s load the required packages.

``` r
library(magrittr)
library(pkgdepR)
```

The required packages have now been added to the search path.

## Single package

Create a `pkgdepR` object as follows:

``` r
v = pkgdepR::deps(pkg = "pkgdepR")
```

We can see a summary of the object

``` r
v # alternatively, summary(v) or print(v)
#> 
#> pkgdepR object
#> ------------------------------
#> Packages:    pkgdepR
#> Total nodes: 15
#> Total links: 18
#>   -Between packages: 0
#>   -Within packages:  18
#>     --Between functions: 17
#>     --Self-referential:  1
```

To see the network visualization, simply call plot.

``` r
plot(dep)
```

## Multiple packages

Visualizing multiple packages works in a similar way.

``` r
v = pkgdepR::deps(pkg = c("pkgdepR", "magrittr"))
```

``` r
v # alternatively, summary(v) or print(v)
#> 
#> pkgdepR object
#> ------------------------------
#> Packages:    pkgdepR, magrittr
#> Total nodes: 67
#> Total links: 48
#>   -Between packages: 7
#>   -Within packages:  41
#>     --Between functions: 39
#>     --Self-referential:  2
```

``` r
plot(v)
```

## Contributors

[Ed Peyton](https://github.com/edpeyton)

------------------------------------------------------------------------

<a href="#top">Back to top</a>
