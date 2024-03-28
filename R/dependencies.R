# builds packages dependency graph from a dependency list
#
# Two packages A and B are connected B->A iff B uses A i.e the relation "->" means "needs"
#
# N.B: the graph will only contain dependencies (as directed edges)
# within the set of packages given as argument
#
#@param deps    a dependencies list
#
#return the dependency graph as an igraph object
build_pkgs_dependency_graph <- function(deps_lst) {
  stop_unless(is.list(deps_lst), 'bad arg "deps_lst", must be a list')
  if (!length(deps_lst)) return(igraph::graph.empty())

  nodes <- names(deps_lst)
  # create the nodes: one for each package
  g <- igraph::graph.empty() + igraph::vertices(nodes)

  edge <- igraph::edge
  for (pkg_name in names(deps_lst)) {
    # N.B: do not consider "external" packages
    pkg_deps <- intersect(deps_lst[[pkg_name]], nodes)
    for (dep in pkg_deps) {
      g <- g + edge(pkg_name, dep)
    }
  }

  g
}

# builds package information in the format of utils::available.packages
# N.B: not all the fields are set
build_pkgs_matrix <- function(pkgs) {
  .parse_dep <- function(pkg, field_name) {
    field <- pkg[[field_name]]
    res <- if (is.null(field)) character() else devtools::parse_deps(field)$name
    paste0(res, collapse = ', ')
  }

  .noname_sapply <- function(...) unname(sapply(..., USE.NAMES = FALSE))

  mat <- cbind(
    Package = .noname_sapply(pkgs, getElement, 'package'),
    LibPath = .noname_sapply(pkgs, getElement, 'path'),
    Version = .noname_sapply(pkgs, getElement, 'version'),
    Priority = NA,
    Depends = .noname_sapply(pkgs, .parse_dep, 'depends'),
    Imports = .noname_sapply(pkgs, .parse_dep, 'imports'),
    LinkingTo = NA,
    Suggests = .noname_sapply(pkgs, .parse_dep, 'suggests'),
    Enhances = NA,
    License = .noname_sapply(pkgs, .parse_dep, 'license'),
    License_is_FOSS = NA,
    License_restricts_use = NA,
    OS_type = NA,
    MD5sum = NA,
    NeedsCompilation= "no",
    Built = NA
  )

  rownames(mat) <- mat[, 1]

  mat
}
