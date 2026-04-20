# instruments the R loaders to make them aware of source packages

hacks [`library()`](https://rdrr.io/r/base/library.html) and
[`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) using the base
R tracer function [`trace()`](https://rdrr.io/r/base/trace.html).
[`library(pkg)`](https://rdrr.io/r/base/library.html) will basically
call `pkg_load(pkg)` if the source package `pkg` is managed by
**srcpkgs**

## Usage

``` r
hack_r_loaders()
```

## Value

no return value, called for side-effects

## Details

N.B: usually you do not need to call that function explicitly. The
function is reentrant.

## Package startup

At package startup (actually `.OnAttach()`), `hack_r_loaders()` will be
automatically called to hack the R loaders, UNLESS this is inhibited via
the option `srcpkgs.inhibit_r_loaders_hack` or the environment variable
`SRCPKGS.INHIBIT_R_LOADERS_HACK`. You may set any value like TRUE,
"TRUE", 1 or "1".

## See also

[`unhack_r_loaders()`](unhack_r_loaders.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# hack library
hack_r_loaders()

# unhack
unhack_r_loaders()

# prevent automatic hacking when srcpkgs is loaded
options(srcpkgs.inhibit_r_loaders_hack=TRUE)
# or
Sys.setenv(SRCPKGS.INHIBIT_R_LOADERS_HACK="1")
library(srcpkgs)
} # }
```
