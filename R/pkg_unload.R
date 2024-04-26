#' unloads a package, unloading some dependent packages if needed
#'
#' To be able to unload properly a package, all the packages that depend
#' even indirectly on it should be unloaded first.
#
#' @inheritParams params
#' @param loaded  the loaded packages, useful for testing.
#' @return a data frame of the unloaded package names, and whether they were
#'  attached, invisibly or NULL if the package is not loaded
#' @export
pkg_unload <- function(pkg_or_name, 
  src_pkgs = get_srcpkgs(), dry_run = FALSE, loaded = loadedNamespaces(), quiet = FALSE)
{
  pkg_name <- as_pkg_name(pkg_or_name)
  # is it a srcpkg or not?
  plan <- NULL
  force(src_pkgs)
  if (pkg_name %in% names(src_pkgs)) {
    plan <- srcpkg_unload_plan(pkg_name, src_pkgs, loaded = loaded)
  } else {
    plan <- non_srcpkg_unload_plan(pkg_name, loaded = loaded)
  }

  if (!dry_run) execute_plan(plan, src_pkgs, quiet = quiet)


  invisible(plan)
}

srcpkg_unload_plan <- function(pkg_name, src_pkgs, 
  loaded = loadedNamespaces(), mat = graph_from_srcpkgs(src_pkgs)) 
{
  if (!pkg_name %in% loaded) return(NULL)
  force(mat)
  # mat <- sub_graph(mat, loaded)
  unload_plan(pkg_name, mat, loaded = loaded)
}

non_srcpkg_unload_plan <- function(pkg_name, loaded = loadedNamespaces()) 
{
  if (!pkg_name %in% loaded) return(NULL)

  deps <- find_loaded_packages_namespace_imports(loaded)
  deps <- lapply(deps, intersect, loaded)
  mat <- graph_from_deps(deps)

  unload_plan(pkg_name, mat, loaded = loaded)
}

# plan to unload pkg_names. will not unload packages not loaded
unload_plan <- function(pkg_names, mat, loaded = loadedNamespaces()) {
  if (!nrow(mat)) return(NULL)
  
  .deps <- function(x) c(graph_get_all_dependents(mat, x), x)
  deps_lst <- lapply(pkg_names, .deps)

  deps <- unique(fast_unlist(deps_lst))
  if (!length(deps)) return(NULL)

  ordering <- graph_topo_sort_nodes(mat, deps)
  plan <- data.frame(package = ordering, action = 'unload')
  plan <- plan[plan$package %in% loaded, , drop = FALSE]

  if (!nrow(plan)) return(NULL)

  plan
}


# N.B: namespace-imports != description-imports
# find the namespace-imports of all loaded packages (but "base" of course) as a dependency list
# does not report "base" as an import as well 
find_loaded_packages_namespace_imports <- function(loaded = loadedNamespaces()) {
  pkg_names <- setdiff(loaded, 'base')

  .fetch_imports <- function(x) {
    y <- pkg_list_ns_imports(x)
    reproducible_sort(setdiff(y, 'base'))
  }
  imports <- lapply(pkg_names, .fetch_imports)
  names(imports) <- pkg_names

  imports[reproducible_sort(names(imports))]
}