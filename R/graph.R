# incidence matrix helper to easily create graph examples
graph_from_strings <- function(...) {
  dots <- list(...)
  seqs <- strsplit(as.character(dots), '->')

  .parse_seq <- function(seq) {
    lapply(seq_len(length(seq) - 1), function(x) c(seq[x], seq[x + 1])) 
  }
  edges <- fast_unlist(lapply(seqs,  .parse_seq))
  nodes <- fast_unlist(seqs)
  graph_from_edges(edges, nodes)
}

graph_from_edges <- function(edges_lst, nodes = fast_unlist(edges_lst)) {
  nodes <- sort(unique(nodes))
  nb <- length(nodes)
  mat <- matrix(0L, nb, nb, dimnames = list(nodes, nodes))
  for (edge in edges_lst) 
    mat[edge[1], edge[2]] <- 1L
  
  mat
}

graph_from_deps <- function(deps_lst) {
  .dep_to_edges <- function(dep) {
    lapply(deps_lst[[dep]], function(x) c(dep, x))
  }
  edges <- fast_unlist(lapply(names(deps_lst), .dep_to_edges))
  nodes <- sort(unique(c(fast_unlist(deps_lst), names(deps_lst))))
  graph_from_edges(edges, nodes)
}


graph_topo_sort <- function(mat) {
  nb <- nrow(mat)

  is_root <- .colSums(mat, nb, nb) == 0
  roots <- colnames(mat)[is_root]
  stop_unless(length(roots), 'error, not a DAG')
  if (length(roots) == nb) return(roots)

  mat2 <- mat[!is_root, !is_root, drop = FALSE]

  c(roots, graph_topo_sort(mat2))
}

graph_topo_sort_nodes <- function(mat, nodes) {
  if (!length(nodes)) return(nodes)
  graph_topo_sort(sub_graph(mat, nodes))
}

# N.B: the dependents are topologically sorted
graph_get_all_dependents <- function(mat, node) {
  if (!node %in% rownames(mat)) return(node)
  rev(graph_get_all_dependencies(t(mat), node))
}


graph_dfs <- function(mat, node,  processed = new.env(parent = emptyenv())) {
  processed[[node]] <- TRUE
  deps <- colnames(mat)[which(mat[node, ] > 0, useNames = FALSE)]
  deps <- setdiff(deps, names(processed))
  for (dep in deps) graph_dfs(mat, dep, processed)
}
graph_get_all_dependencies <- function(mat, node) {
  if (!node %in% rownames(mat)) return(character())
  processed <- new.env(parent = emptyenv())
  graph_dfs(mat, node, processed)

  deps <- setdiff(names(processed), node)
  graph_topo_sort_nodes(mat, deps)
}



# keep only given nodes
sub_graph <- function(mat, nodes) {
  keep <- intersect(rownames(mat), nodes)
  mat[keep, keep, drop = FALSE]
}

