# finds all available source packages starting from the project root

N.B: the *hidden* files and directories are ignored. In general, this
function is not used directly, instead you should use
[`get_srcpkgs()`](get_srcpkgs.md)

## Usage

``` r
find_srcpkgs(
  root = get_project_root(),
  srcpkgs_paths = find_srcpkgs_paths(root, prune = prune),
  prune = TRUE
)
```

## Arguments

- root:

  directory from where to search for source packages

- srcpkgs_paths:

  paths to the source packages folders

- prune:

  whether to report packages contained inside another package (e.g. in
  tests/)

## Value

a "srcpkgs" object (or NULL if none found), a named list of "srcpkg"
objects, that essentially are devtools "package" objects. The list is
named after the package names.

## Examples

``` r
pkg <- setup_and_get_dummy_srcpkg()
pkgs <- find_srcpkgs(dirname(pkg$path))
print(pkgs)
#>                   package version                                          path
#> dummy.srcpkg dummy.srcpkg  3.1416 /tmp/Rtmp7TeFwM/file1bba4eabba26/dummy_srcpkg
#>              imports depends suggests
#> dummy.srcpkg                 testthat
```
