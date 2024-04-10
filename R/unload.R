
# find the imports of all loaded packages (but "base" of course) as a dependency list
# does not report "base" as an import as well 
find_loaded_packages_imports <- function() {
  pkg_names <- setdiff(loadedNamespaces(), 'base')

  .fetch_imports <- function(x) {
    y <- pkg_list_ns_imports(x)
    reproducible_sort(setdiff(y, 'base'))
  }
  imports <- lapply(pkg_names, .fetch_imports)
  names(imports) <- pkg_names

  imports <- imports[lengths(imports) > 0]

  imports[reproducible_sort(names(imports))]
}