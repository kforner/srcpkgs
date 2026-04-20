# lists the packages that are attached, i.e. present in the R search() path

lists the packages that are attached, i.e. present in the R search()
path

## Usage

``` r
pkg_list_attached()
```

## Value

the names of attached package name as a character vector

## Examples

``` r
print(sort(pkg_list_attached()))
#> [1] "base"      "datasets"  "grDevices" "graphics"  "methods"   "srcpkgs"  
#> [7] "stats"     "utils"    
```
