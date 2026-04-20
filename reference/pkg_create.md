# creates and populates a R package-like folder programmatically, useful for writing tests

basically a wrapper around
[`utils::package.skeleton()`](https://rdrr.io/r/utils/package.skeleton.html)

## Usage

``` r
pkg_create(
  dir,
  name,
  functions = list(dummy = function() "DUMMY"),
  imports = NULL,
  depends = NULL,
  suggests = NULL,
  namespace = FALSE,
  roxygen_imports = FALSE,
  ignore_hidden_files = TRUE
)
```

## Arguments

- dir:

  the directory in which to create the package, as a string

- name:

  the package name, as a string

- functions:

  a named list of functions to add to the package

- imports:

  the "imports" dependencies

- depends:

  the "depends" dependencies

- suggests:

  the "suggests" dependencies

- namespace:

  whether to write the namespace file (currently only applicable to the
  imports. N.B: if the namespace file is generated, roxygen will refuse
  to update it

- roxygen_imports:

  whether to write the roxygen statements to defined the imports

- ignore_hidden_files:

  whether to create a `.Rbuildignore` file to ignore hidden files.

## Value

the srcpkg instance, invisibly
