
#' computes the dependencies of a (source) package
#' 
#' 

pkg_deps <- function(pkgid, src_pkgs = find_srcpkgs(), 
  source = TRUE, installed = TRUE, 
  imports = TRUE, depends = TRUE, suggests = TRUE, 
  reverse = FALSE) 
{
  .get_pkg_deps <- function(pkg) {
    lst <- get_srcpkg_dependencies(pkg)
    if (!imports) lst$imports <- NULL
    if (!depends) lst$depends <- NULL
    if (!suggests) lst$suggests <- NULL
    fast_unlist(lst)
  }

  srcpkgs_deps <- lapply(src_pkgs, .get_pkg_deps)
  mat <- graph_from_deps(srcpkgs_deps)
  
  deps <- if (reverse) {
    graph_get_all_dependents(pkgid, mat = mat)
  } else { 
    graph_get_all_dependencies(pkgid, mat = mat)
  }

  if (!source) deps <- setdiff(deps, names(src_pkgs))
  if (!installed) deps <- intersect(deps, names(src_pkgs))

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
