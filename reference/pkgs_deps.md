# computes the dependencies of some (source) packages

computes the dependencies of some (source) packages

## Usage

``` r
pkgs_deps(
  pkgids,
  src_pkgs = get_srcpkgs(),
  source = TRUE,
  installed = TRUE,
  imports = TRUE,
  depends = TRUE,
  suggests = TRUE,
  reverse = FALSE
)
```

## Arguments

- pkgids:

  a list of package ids (names, paths or object), or a srcpkgs object.
  Also accept a singleton package object

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- source:

  whether to report source packages

- installed:

  whether to report installed (non-source) packages

- imports:

  whether to only consider `imports` dependencies

- depends:

  whether to only consider `depends` dependencies

- suggests:

  whether to only consider `suggests` dependencies

- reverse:

  whether to compute reverse dependencies instead

## Value

the dependencies, as a character vector, topologically sorted

## Examples

``` r
pkg <- setup_and_get_dummy_srcpkg()
deps_src <- pkgs_deps(pkg, installed = FALSE)
deps_inst <- pkgs_deps(pkg, source = FALSE)
print(get_srcpkgs())
#>                   package version                                          path
#> dummy.srcpkg dummy.srcpkg  3.1416 /tmp/Rtmp2CbWDw/file1ac0572d9c49/dummy_srcpkg
#>              imports depends suggests
#> dummy.srcpkg                 testthat
deps_rev <- pkgs_deps(pkg, reverse = TRUE, suggests = FALSE)
```
