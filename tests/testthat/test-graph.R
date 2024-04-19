test_that("graph_from_strings", {
  mat <- graph_from_strings('A->B', 'B->C', 'D->C')

  expect_equal(dim(mat), c(4, 4))
  expect_equal(sum(mat), 3)
  expect_equal(mat['B', 'C'], 1)
})


test_that("graph_topo_sort", {
  mat <- matrix(0, 1, 1, dimnames = list('A', 'A'))
  expect_identical(graph_topo_sort(mat), 'A')

  ## A->B
  mat <- graph_from_strings('A->B')
  expect_identical(graph_topo_sort(mat), c('A', 'B'))

  ## A->B->C
  mat <- graph_from_strings('A->B', 'B->C')
  expect_identical(graph_topo_sort(mat), c('A', 'B', 'C'))

  # A->B->C, B->D->C
  mat <- graph_from_strings('A->B', 'B->C', 'B->D', 'D->C')
  expect_identical(graph_topo_sort(mat), c('A', 'B', 'D', 'C'))

  ## cycle
  mat <- graph_from_strings('A->B', 'B->A')
  expect_error(graph_topo_sort(mat), 'not a DAG')
})


test_that("graph_get_all_dependents", {
  ### A->B
  mat <- graph_from_strings('A->B')
  expect_identical(graph_get_all_dependents(mat, 'A'), 'A')

  expect_identical(graph_get_all_dependents(mat, 'B'), c('B', 'A'))

  # not in the graph
  expect_identical(graph_get_all_dependents(mat, 'D'), 'D')

  ### A->B->C
  mat <- graph_from_strings('A->B', 'B->C')

  expect_identical(graph_get_all_dependents(mat, 'B'), c('B', 'A'))
  expect_identical(graph_get_all_dependents(mat, 'C'), c('C', 'B', 'A'))

  ### more complex example
  mat <- graph_from_strings('A->B', 'B->C', 'A->D', 'D->C', 'D->B')

  expect_identical(graph_get_all_dependents(mat, 'C'), c('C', 'B', 'D', 'A'))
  expect_identical(graph_get_all_dependents(mat, 'B'), c('B', 'D', 'A'))
  expect_identical(graph_get_all_dependents(mat, 'D'), c('D', 'A'))

  ### not a DAG
  mat <- graph_from_strings('A->B', 'B->A')
  expect_error(graph_get_all_dependents(mat, 'A'), 'not a DAG')
})
