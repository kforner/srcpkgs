# unloads a package, unloading its dependent packages if needed

To be able to unload properly a package, all the packages that depend
even indirectly on it should be unloaded first.

## Usage

``` r
pkg_unload(
  pkg_or_name,
  src_pkgs = get_srcpkgs(),
  dry_run = FALSE,
  loaded = loadedNamespaces(),
  quiet = FALSE
)
```

## Arguments

- pkg_or_name:

  a package name or object ("package" or "srcpkg")

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- dry_run:

  whether not to actually execute any action having side-effects

- loaded:

  the loaded packages, useful for testing.

- quiet:

  whether to be quiet/silent

## Value

a data frame of the unloaded package names, and whether they were
attached, invisibly or NULL if the package is not loaded

## Details

N.B: this function also works for non source packages.

## Examples

``` r
root <- tempfile()
pkg <- setup_and_get_dummy_srcpkg(root)
reset(root)
pkg_load(pkg)
#> executing unload on dummy.srcpkg
#> executing load on dummy.srcpkg
#> ℹ Updating dummy.srcpkg documentation
#> First time using roxygen2. Upgrading automatically...
#> ℹ Setting RoxygenNote to "7.3.3"
#> ℹ Loading dummy.srcpkg
#> Writing NAMESPACE
#> ℹ Loading dummy.srcpkg

pkg_unload(pkg)
#> executing unload on dummy.srcpkg
```
