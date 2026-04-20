# resets the `srcpkgs` settings

With this function, you can reset or set precisely the settings.

## Usage

``` r
reset(root = find_project_root(), srcpkgs_paths = find_srcpkgs_paths(root))
```

## Arguments

- root:

  directory from where to search for source packages

- srcpkgs_paths:

  paths to the source packages folders

## Value

the settings (cf [`settings()`](settings.md)) invisibly

## Examples

``` r
# reset to appropriate defaults based on your current directory
old <- reset()

# explictly set the project root
reset(root = tempdir())

# explictly set the source package paths (very unlikely)
reset(srcpkgs_paths = c('pkgs/mypkg1', 'pkgs/mypkg2'))

# restore previous settings
reset(root = old$root, srcpkgs_paths = old$srcpkgs_paths)
```
