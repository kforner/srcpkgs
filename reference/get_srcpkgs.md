# get the current source packages list

The first call to this function will trigger the initialization of the
package ((cf [`reset()`](reset.md)). Since it is used by mostly all
user-facing load-related functions, this enables a runtime
initialization, as opposed to a load-time initialization. So for example
you may load `srcpkgs`, then change the current directory to your
project. Then the first load will setup the settings.

## Usage

``` r
get_srcpkgs(filter = NULL)
```

## Arguments

- filter:

  a pattern to filter the source packages

## Value

the source packages as a "scrpkgs" object, cf
[`find_srcpkgs()`](find_srcpkgs.md), or NULL if none

## Details

For optimization, the paths to discovered source packages are cached (cf
[`reset()`](reset.md) and [`settings()`](settings.md). This function
will reparse the DESCRIPTION for any change. If you add or delete a
source package, you must reset the source package paths using
[`reset()`](reset.md)

This function is useful for troubleshooting, to understand what are the
source packages discovered and managed by `srcpkgs`

## Examples

``` r
# setup a srcpkg. We need reset because it is not discoverable from the current directory
pkg <- setup_and_get_dummy_srcpkg()
reset(dirname(pkg$path))

print(get_srcpkgs())
#>                   package version                                         path
#> dummy.srcpkg dummy.srcpkg  3.1416 /tmp/Rtmp2CbWDw/file1ac0c975314/dummy_srcpkg
#>              imports depends suggests
#> dummy.srcpkg                 testthat
```
