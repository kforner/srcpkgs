# informs about the settings currently used by `srcpkgs`

informs about the settings currently used by `srcpkgs`

## Usage

``` r
settings()
```

## Value

a named list of:

- initialized: whether the settings are initialized (as triggered by
  [`get_srcpkgs()`](get_srcpkgs.md))

- root: the project root

- srcpkgs_paths: the paths of the source packages to manage

- hack_r_loaders_installed: whether the R loaders are hacked

- hack_r_loaders_enabled: whether the R loaded hack is in action
  (internal use)

## Examples

``` r
print(settings())
#> $initialized
#> [1] TRUE
#> 
#> $root
#> [1] "/home/runner/work/srcpkgs/srcpkgs"
#> 
#> $srcpkgs_paths
#> [1] "/home/runner/work/srcpkgs/srcpkgs"
#> 
#> $hack_r_loaders_installed
#> [1] TRUE
#> 
#> $hack_r_loaders_enabled
#> [1] TRUE
#> 
```
