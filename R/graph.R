# incidence matrix helper to easily create graph examples
graph_from_strings <- function(...) {
  dots <- list(...)
  edges <- unlist(lapply(dots, strsplit, split = '->'), recursive = FALSE, use.names = FALSE)
  nodes <- sort(unique(unlist(edges, recursive = FALSE, use.names = FALSE)))

  nb <- length(nodes)
  mat <- matrix(0L, nb, nb, dimnames = list(nodes, nodes))

  for (edge in edges) mat[edge[1], edge[2]] <- 1L

  mat
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

graph_get_all_dependents <- function(mat, node) {
  if (!node %in% rownames(mat)) return(node)
  mat2 <- t(mat)
  nodes <- graph_topo_sort(mat2)
  # cut that list at our node
  idx <- which(nodes == node)
  nodes[idx:length(nodes)]

  # nb <- nrow(mat)
  # if (nb <= 1) return(node)
  
  # mat2 <- t(mat)
  # # remove all roots but node: trick make node->node edge so that our node is not a root
  # mat2[node, node] <- 1L
  # is_root <- .colSums(mat2, nb, nb) == 0
  # mat2[node, node] <- 0L

  # mat3 <- mat2[!is_root, !is_root, drop = FALSE]

  # graph_topo_sort(mat3)
}