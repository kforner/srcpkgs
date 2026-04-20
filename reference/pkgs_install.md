# installs a list of source packages

- A source package can not be installed if its dependencies are not.

- Will not reinstall packages if they are up-to-date

- will roxygenise packages if needed

## Usage

``` r
pkgs_install(
  pkgids,
  lib,
  src_pkgs = get_srcpkgs(),
  only_deps = FALSE,
  quiet = TRUE,
  ...
)
```

## Arguments

- pkgids:

  a list of package ids (names, paths or object), or a srcpkgs object.
  Also accept a singleton package object

- lib:

  directory where to install and find installed pkgs

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- only_deps:

  whether not to include `pkgids`, only their dependencies.

- quiet:

  whether to be quiet/silent

- ...:

  passed to
  [`devtools::install()`](https://devtools.r-lib.org/reference/install.html)

## Value

the names of the packages actually installed
