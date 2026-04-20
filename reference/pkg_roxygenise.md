# roxygenize a source package if needed

- if the package has not changed (based on the md5sum file), does
  nothing

- otherwise roxygenise the package using roxygen2::roxygenise

- and update and save the new md5sum file

## Usage

``` r
pkg_roxygenise(pkg_path, force = FALSE, quiet = FALSE, ...)
```

## Arguments

- pkg_path:

  the package path, as a character

- force:

  if force(d), do not use the md5-based system to detect package changes

- quiet:

  whether to be quiet/silent

- ...:

  passed to
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)

## Value

if the roxygenation has been performed

## Details

- N.B: has the side-effect of loading the package

## Examples

``` r
 pkg <- setup_and_get_dummy_srcpkg()
 pkg_roxygenise(pkg$path)
#> ℹ Updating dummy.srcpkg documentation
#> First time using roxygen2. Upgrading automatically...
#> ℹ Setting RoxygenNote to "7.3.3"
#> ℹ Loading dummy.srcpkg
#> Writing NAMESPACE
```
