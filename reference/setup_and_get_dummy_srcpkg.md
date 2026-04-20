# installs the dummy srcpkg in a temp location

Intended for testing and to write examples

## Usage

``` r
setup_and_get_dummy_srcpkg(dest = tempfile())
```

## Arguments

- dest:

  where to install the dummy srcpkg

## Value

the package as a `srcpkg` object

## Examples

``` r
pkg <- setup_and_get_dummy_srcpkg()
print(pkg)
#> dummy.srcpkg@<3.1416> source package
#> [/tmp/Rtmp2CbWDw/file1ac05d0c2a44/dummy_srcpkg]
```
