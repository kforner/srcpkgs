# loads or reloads if needed a source package, taking care of its dependencies

N.B: the defaults are different from
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html):
the helpers are not loaded, only the functions tagged as *exported* are
actually exported. The intended goal is to make it as similar to the
behaviour of the R loaders.

## Usage

``` r
pkg_load(
  pkgid,
  src_pkgs = get_srcpkgs(),
  attach = TRUE,
  suggests = FALSE,
  roxygen = TRUE,
  helpers = FALSE,
  export_all = FALSE,
  quiet = FALSE,
  dry_run = FALSE,
  ...
)
```

## Arguments

- pkgid:

  a package name, path or package object

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- attach:

  Whether to attach a package environment to the search path. If `FALSE`
  `load_all()` behaves like
  [`loadNamespace()`](https://rdrr.io/r/base/ns-load.html). If `TRUE`
  (the default), it behaves like
  [`library()`](https://rdrr.io/r/base/library.html). If `FALSE`, the
  `export_all`, `export_imports`, and `helpers` arguments have no
  effect.

- suggests:

  whether to load suggested packages. if TRUE, the suggested are
  processed like imports

- roxygen:

  whether to automatically roxygenise packages (if needed)

- helpers:

  if `TRUE` loads testthat test helpers.

- export_all:

  If `TRUE` (the default), export all objects. If `FALSE`, export only
  the objects that are listed as exports in the NAMESPACE file.

- quiet:

  whether to be quiet/silent

- dry_run:

  whether not to actually execute any action having side-effects

- ...:

  Arguments passed on to
  [`devtools::load_all`](https://devtools.r-lib.org/reference/load_all.html)

  `path`

  :   Path to a package, or within a package.

  `reset`

  :   **\[deprecated\]** This is no longer supported because preserving
      the namespace requires unlocking its environment, which is no
      longer possible in recent versions of R.

  `recompile`

  :   DEPRECATED. force a recompile of DLL from source code, if present.
      This is equivalent to running
      [`pkgbuild::clean_dll()`](https://pkgbuild.r-lib.org/reference/clean_dll.html)
      before `load_all()`

## Value

the load plan as a data frame, or NULL if there is nothing to do.

## Details

This the workhorse function of the package, called by
[`library()`](https://rdrr.io/r/base/library.html) and
[`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) when hacked (cf
[`hack_r_loaders()`](hack_r_loaders.md).

This function will check that all dependent packages are up-to-date, and
document and reload them as needed.

To be able to properly load a package, its dependent source packages
must be loaded in proper order. i.e. if A–\>B–\>C, the load order must
be C, B, A

## Examples

``` r
root <- tempfile()
pkg <- setup_and_get_dummy_srcpkg(root)
reset(root)
# load and attach a package
pkg_load(pkg)
#> executing load on dummy.srcpkg
#> ℹ Updating dummy.srcpkg documentation
#> First time using roxygen2. Upgrading automatically...
#> ℹ Setting RoxygenNote to "7.3.3"
#> ℹ Loading dummy.srcpkg
#> Writing NAMESPACE
#> ℹ Loading dummy.srcpkg

# just load, do not attach it (~ loadNamespace())
pkg_unload(pkg)
#> executing unload on dummy.srcpkg
pkg_load(pkg, attach = FALSE)
#> executing load on dummy.srcpkg
#> ℹ Loading dummy.srcpkg

# do some changes, to a source package or any of its depencies or dependents
pkg_unload(pkg)
#> executing unload on dummy.srcpkg
plan <- pkg_load(pkg, dry_run = TRUE)
# then you can inspect the plan actions
```
