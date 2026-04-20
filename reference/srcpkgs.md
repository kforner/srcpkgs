# creates a new "srcpkgs" object

creates a new "srcpkgs" object

## Usage

``` r
srcpkgs(pkgs = lapply(paths, devtools::as.package), paths = NULL)
```

## Arguments

- pkgs:

  an existing srcpkgs object (no op), or a list of source package-like
  objects

- paths:

  a list of source package paths as a character vector or list

## Value

a `srcpkgs` object (a list named after the package names)

## Examples

``` r
# build dummy source packages
pkg1 <- setup_and_get_dummy_srcpkg()
pkg2 <- pkg1
pkg2$package <- "dummy.srcpkg2"

print(srcpkgs(list(pkg1, pkg2)))
#>                     package version
#> dummy.srcpkg   dummy.srcpkg  3.1416
#> dummy.srcpkg2 dummy.srcpkg2  3.1416
#>                                                        path imports depends
#> dummy.srcpkg  /tmp/Rtmp7TeFwM/file1bba3345ab1e/dummy_srcpkg                
#> dummy.srcpkg2 /tmp/Rtmp7TeFwM/file1bba3345ab1e/dummy_srcpkg                
#>               suggests
#> dummy.srcpkg  testthat
#> dummy.srcpkg2 testthat
print(srcpkgs(paths = pkg1$path))
#>                   package version                                          path
#> dummy.srcpkg dummy.srcpkg  3.1416 /tmp/Rtmp7TeFwM/file1bba3345ab1e/dummy_srcpkg
#>              imports depends suggests
#> dummy.srcpkg                 testthat
```
