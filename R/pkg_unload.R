#' unloads a package, unloading some dependent packages if needed
#'
#' To be able to unload properly a package, all the packages that depend
#' even indirectly on it should be unloaded first.
#
#' @inheritParams params
#' @return a data frame of the unloaded package names, and whether they were
#'  attached, invisibly or NULL if the package is not loaded
#' @export
pkg_unload <- function(pkg_or_name, quiet = FALSE)
{
  pkg_name <- as_pkg_name(pkg_or_name)
  if (!pkg_is_loaded(pkg_name)) return(invisible())

  info <- if(quiet) function(...) {} else cli::cli_text

  pkgs_to_unload <- pkg_name

  ## add dependent packages
  nsimports <- find_loaded_packages_namespace_imports()
  deps_graph <- build_pkgs_dependency_graph(nsimports)
  dependents <- igraph_list_node_dependents(pkg_name, deps_graph)
    # process them in the reverse order of the load order
  pkgs_to_unload <- c(rev(dependents), pkgs_to_unload)
  
  df <- data.frame(
    pkg = pkgs_to_unload,
    attached = sapply(pkgs_to_unload, pkg_is_attached), 
    stringsAsFactors = FALSE)
  
  for (pkg in pkgs_to_unload) {
    info('Unloading package {.pkg {pkg}}...')
    # N.B: we prefer to use unregister since it is faster than unload
    pkgload::unregister(pkg)
    #devtools::unload(pkg)
  }

  invisible(df)
}

srcpkg_unload <- function(pkg_name, src_pkgs = get_srcpkgs(), quiet = FALSE, dry_run = FALSE) {
  # consider only loaded srcpkgs
  # keep only pkg_name and its dependents
  # list them in topological order
}


# N.B: namespace-imports != description-imports
# find the namespace-imports of all loaded packages (but "base" of course) as a dependency list
# does not report "base" as an import as well 
find_loaded_packages_namespace_imports <- function() {
  pkg_names <- setdiff(loadedNamespaces(), 'base')

  .fetch_imports <- function(x) {
    y <- pkg_list_ns_imports(x)
    reproducible_sort(setdiff(y, 'base'))
  }
  imports <- lapply(pkg_names, .fetch_imports)
  names(imports) <- pkg_names

  imports[reproducible_sort(names(imports))]
}