  # srcpkgs

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/kforner/srcpkgs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kforner/srcpkgs/actions/workflows/R-CMD-check.yaml)

  [![Codecov test coverage](https://codecov.io/gh/kforner/srcpkgs/branch/main/graph/badge.svg)](https://app.codecov.io/gh/kforner/srcpkgs?branch=main)
  <!-- badges: end -->

The aim of `srcpkgs` is to make development and use of source packages easy, and thus encourage to  continuously develop a shared library of reusable code. 

## Synopsis

Developing a library of packages can be really tedious, especially if there are dependencies between those packages. Each time you change a package, you have to **install** it before being able to use it.

The advent of [devtools](https://devtools.r-lib.org/) enabled the use of source packages directly, bypassing the installation step, which considerably streamlined the development of ONE package, also making it possible to reload a package within a R session.

`srcpkgs` *extends* devtools to manage the development and use of a  collection or library of source packages as easily and efficiently as possible.
In particular, it understands the dependencies between those packages and upon changes, determine and apply the optimal
course of action to apply those changes.

See the vignettes:

-  [Why would you need srcpkgs?](https://kforner.github.io/srcpkgs/articles/demo.html)
- [Getting Started](https://kforner.github.io/srcpkgs/articles/getting_started.html)

## intended target

The main use case for `srcpkgs` is for an organization with many R users, that want to share and reuse code across projects and users. This reusable code will be implemented as a collection of R source packages. The project code itself should also be implemented as a source package. 
This way: 
- each project has it own version of the collection
- the project devs can easily adapt the collection for their project (add features, fix bugs) in the same R session used for the project
- at any time, the project devs can **push** their changes to the collection


The recommended setup is:
- one git repository for the (interdependent) collection of source packages
  * of course you could have a git repo per source package, but if you have dozends of them it's cumbersome to add them as git sub modules to all your projects. Moreover it's harder to select compatible versions.
- one git repository for a project
   * with the collection add the the project as a **git submodule**


This can also be useful for a single developer. This will allow to easily reuse code across projects, avoiding the duplication of code. 

**N.B**: wrapping the code as a source package is for convenience, and is really easy. It's useful even if you have no plan of releasing or distributing the package. 


## Features

- fast and automatic **discovery** of the source packages 
- fast and automatic detection of **changes**: quickly unload/reload/load only what is needed.
- apply roxygen ([cf roxygen2](https://roxygen2.r-lib.org/)) automatically and only when needed
- can **hack** the R loaders (`library()` and `loadNamespace()`) to use `srcpkgs` in development, 
  so that your scripts still work if you install your packages (e.g. in *Production* mode)
- extensively tested (~ 450 tests) and 100% test coverage

### forthcoming

- **testing** the collection at once, in parallel, and output an aggregated report (using [testthat](https://testthat.r-lib.org/) of course).
- same for `R CMD check`ing the collection
- same for test coverage (using [covr](https://covr.r-lib.org/)).


## Installation

The package is not (yet) available on CRAN.

Install it from github: 
```
### using devtools
# install devtools from CRAN if needed
install.packages('devtools') 

install_github('kforner/srcpkgs')

### or using pak
# install pak from CRAN if needed
install.packages('pak') 
pak::pak("kforner/srcpkgs")
```

