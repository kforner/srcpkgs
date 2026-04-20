# tests a package - runs R CMD check

This function will check a source package.

## Usage

``` r
pkg_check(
  pkgid,
  src_pkgs = get_srcpkgs(),
  lib = ".check",
  roxygen = TRUE,
  quiet = FALSE,
  error_on = "error",
  check_system_clock = FALSE,
  ...
)
```

## Arguments

- pkgid:

  a package name, path or package object

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- lib:

  directory where to install and find installed pkgs

- roxygen:

  whether to roxygenize

- quiet:

  whether to be quiet/silent

- error_on:

  passed to
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)

- check_system_clock:

  if FALSE, disable the `_R_CHECK_SYSTEM_CLOCK_` check. This check
  sometimes fail because of firewalls...

- ...:

  passed to
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)

## Value

the results as a `pkg_test` object, or NULL if no tests found
