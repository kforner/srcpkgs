# checks a list of source packages

checks a list of source packages

## Usage

``` r
pkgs_check(
  pkgids = names(filter_srcpkgs(src_pkgs, filter)),
  src_pkgs = get_srcpkgs(),
  filter = NULL,
  lib = ".check",
  quiet = FALSE,
  fail_on_error = FALSE,
  ...
)
```

## Arguments

- pkgids:

  a list of package ids (names, paths or object), or a srcpkgs object.
  Also accept a singleton package object

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- filter:

  filter out the packages to check using this pattern

- lib:

  directory where to install and find installed pkgs

- quiet:

  whether to be quiet/silent

- fail_on_error:

  whether to die if there is at least an error or warning in the checks

- ...:

  passed to `pkg_check`

## Value

the results as a `pkgs_test` object
