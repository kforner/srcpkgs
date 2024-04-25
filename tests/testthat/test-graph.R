test_that("graph_from_edges", {
  ### A->B, A->C, B->C, B->D
  edges <- list(c('A', 'B'), c('A', 'C'), c('B', 'D'), c('B', 'C'))
  mat <- graph_from_edges(edges)

  expect_identical(mat, graph_from_deps(list(A = c('B', 'C'), B = c('D', 'C'))))
})


test_that("graph_from_deps", {
  ### single node, no edge
  mat <- graph_from_deps(list(A = NULL))
  expect_identical(mat, matrix(0L, 1, 1, dimnames = list('A', 'A')))

  ### A->A
  mat <- graph_from_deps(list(A = 'A'))
  expect_identical(mat, matrix(1L, 1, 1, dimnames = list('A', 'A')))

  ### A->B
  mat <- graph_from_deps(list(A = 'B'))

  expect_equal(dim(mat), c(2, 2))
  expect_equal(mat['A', 'B'], 1)
  expect_equal(sum(mat), 1)

  ### A->B, A->C, B->C, B->D
  mat <- graph_from_deps(list(A = c('B', 'C'), B = c('D', 'C')))

  nodes <- LETTERS[1:4]
  expect_identical(mat, 
    matrix(
      as.integer(c(0, 1, 1, 0,
        0, 0, 1, 1, 
        0, 0, 0, 0,
        0, 0, 0, 0)),
      4, 4, byrow = TRUE, dimnames = list(nodes, nodes)))
})

test_that("graph_from_strings", {
  ###
  mat <- graph_from_strings('A->B', 'B->C', 'D->C')
  expect_identical(mat, graph_from_edges(list(c('A', 'B'), c('B', 'C'), c('D', 'C'))))

  ### combined
  mat <- graph_from_strings('A->B->C', 'B->C->D')
  expect_identical(mat, graph_from_edges(list(c('A', 'B'), c('B', 'C'), c('B', 'C'), c('C', 'D'))))

  ### isolated nodes
  mat <- graph_from_strings('A->B->C', 'D')
  expect_identical(mat, graph_from_deps(list(A = 'B', B = 'C', D = NULL)))
})


test_that("graph_topo_sort", {
  mat <- matrix(0, 1, 1, dimnames = list('A', 'A'))
  expect_identical(graph_topo_sort(mat), 'A')

  ## A->B
  mat <- graph_from_strings('A->B')
  expect_identical(graph_topo_sort(mat), c('A', 'B'))

  ## A->B->C
  mat <- graph_from_strings('A->B->C')
  expect_identical(graph_topo_sort(mat), c('A', 'B', 'C'))

  # A->B->C, B->D->C
  mat <- graph_from_strings('A->B->C', 'B->D->C')
  expect_identical(graph_topo_sort(mat), c('A', 'B', 'D', 'C'))

  ## cycle
  mat <- graph_from_strings('A->B', 'B->A')
  expect_error(graph_topo_sort(mat), 'not a DAG')
})


test_that("graph_get_all_dep*", {
  ## A->B, C->D
  mat <- graph_from_strings('A->B', 'C->D')

  expect_identical(graph_get_all_dependencies(mat, 'A'),  'B')
  expect_identical(graph_get_all_dependencies(mat, 'B'), character())
  expect_identical(graph_get_all_dependencies(mat, 'C'),  'D')
  expect_identical(graph_get_all_dependencies(mat, 'D'), character())

  expect_identical(graph_get_all_dependents(mat, 'B'), 'A')
  expect_identical(graph_get_all_dependents(mat, 'D'), 'C')
  expect_identical(graph_get_all_dependents(mat, 'A'), character())
  expect_identical(graph_get_all_dependents(mat, 'C'), character())

  ### A->B, C
  mat <- graph_from_strings('A->B', 'C->C')
  mat['C', 'C'] <- 0L

  expect_identical(graph_get_all_dependencies(mat, 'A'),  'B')
  expect_identical(graph_get_all_dependencies(mat, 'B'), character())
  expect_identical(graph_get_all_dependencies(mat, 'C'), character())

  expect_identical(graph_get_all_dependents(mat, 'B'), 'A')
  expect_identical(graph_get_all_dependents(mat, 'A'), character())
  expect_identical(graph_get_all_dependents(mat, 'C'), character())

  ### A->B->C
  mat <- graph_from_strings('A->B->C')

  expect_identical(graph_get_all_dependencies(mat, 'A'),  c('B', 'C'))
  expect_identical(graph_get_all_dependencies(mat, 'B'), 'C')
  expect_identical(graph_get_all_dependencies(mat, 'C'), character())

  expect_identical(graph_get_all_dependents(mat, 'A'), character())
  expect_identical(graph_get_all_dependents(mat, 'B'), 'A')
  expect_identical(graph_get_all_dependents(mat, 'C'), c('A', 'B'))

  ### A->B->C, A->C
  mat <- graph_from_strings('A->B', 'C->B', 'A->C')

  expect_identical(graph_get_all_dependencies(mat, 'A'),  c('C', 'B'))
  expect_identical(graph_get_all_dependencies(mat, 'B'), character())
  expect_identical(graph_get_all_dependencies(mat, 'C'), 'B')

  expect_identical(graph_get_all_dependents(mat, 'A'), character())
  expect_identical(graph_get_all_dependents(mat, 'B'), c('A', 'C'))
  expect_identical(graph_get_all_dependents(mat, 'C'), 'A')

  # let's rotate it B->C, B->A, C->A
  new_names <- c('B', 'A', 'C')
  dimnames(mat) <- list(new_names, new_names)

  expect_identical(graph_get_all_dependencies(mat, 'B'),  c('C', 'A'))
  expect_identical(graph_get_all_dependents(mat, 'A'), c('B', 'C'))

  ### more complex example
  mat <- graph_from_strings('A->B->C', 'A->D->B', 'D->C')

  expect_identical(graph_get_all_dependencies(mat, 'A'), c('D', 'B', 'C'))
  expect_identical(graph_get_all_dependencies(mat, 'D'), c('B', 'C'))

  expect_identical(graph_get_all_dependents(mat, 'C'), c('A', 'D', 'B'))
  expect_identical(graph_get_all_dependents(mat, 'B'), c('A', 'D'))
  expect_identical(graph_get_all_dependents(mat, 'D'), 'A')

  ### not a DAG
  mat <- graph_from_strings('A->B->A')

  expect_identical(graph_get_all_dependencies(mat, 'A'), 'B')
  expect_identical(graph_get_all_dependents(mat, 'A'), 'B')
})
