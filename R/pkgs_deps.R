
#' computes the dependencies of some (source) packages
#' 
#' @inheritParams params
#' @param source      whether to report source packages
#' @param installed   whether to report installed (non-source) packages
#' @param imports     whether to only consider `imports` dependencies
#' @param depends     whether to only consider `depends` dependencies
#' @param suggests    whether to only consider `suggests` dependencies
#' @param reverse     whether to compute reverse dependencies instead
#' 
#' @return the dependencies, as a character vector, topologically sorted
#' @export
#' @examples
#' pkg <- setup_and_get_dummy_srcpkg()
#' deps_src <- pkgs_deps(pkg, installed = FALSE)
#' deps_inst <- pkgs_deps(pkg, source = FALSE)
#' print(get_srcpkgs())
#' deps_rev <- pkgs_deps(pkg, reverse = TRUE, suggests = FALSE)
pkgs_deps <- function(pkgids, src_pkgs = get_srcpkgs(), 
  source = TRUE, installed = TRUE, 
  imports = TRUE, depends = TRUE, suggests = TRUE, 
  reverse = FALSE) 
{
  .get_pkgs_deps <- function(pkg) {
    lst <- get_srcpkg_dependencies(pkg)
    if (!imports) lst$imports <- NULL
    if (!depends) lst$depends <- NULL
    if (!suggests) lst$suggests <- NULL
    fast_unlist(lst)
  }

  srcpkgs_deps <- lapply(src_pkgs, .get_pkgs_deps)
  mat <- graph_from_deps(srcpkgs_deps)
  
  .fetch_deps <- if (reverse) graph_get_all_dependents else graph_get_all_dependencies
  deps <- fast_unlist(lapply(pkgids, .fetch_deps, mat = mat))

  if (!source) deps <- setdiff(deps, names(src_pkgs))
  if (!installed) deps <- intersect(deps, names(src_pkgs))

  deps <- graph_topo_sort_nodes(mat, deps)

  if (reverse) deps <- rev(deps)

  deps
}


get_srcpkg_dependencies <- function(src_pkg) {
  .parse_deps_field <- function(type) {
    str <- gsub('\n', '', src_pkg[[type]])
    if (length(str) == 0 || !nzchar(str)) return(NULL)
    strsplit(str, ',')[[1]]
  }

  sapply(c('imports', 'depends', 'suggests'), .parse_deps_field, simplify = FALSE)
}
