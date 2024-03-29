test_that("build_pkgs_matrix", {
  build_pkgs_matrix <- srcpkgs:::build_pkgs_matrix
  setup_temp_dir()

  pkg_create('.', 'AA', depends = 'BB')
  pkg_create('.', 'BB', imports = 'CC', depends = 'CC')
  pkg_create('.', 'CC', suggests = 'DD')
  pkg_create('.', 'DD')
  pkgs <- lapply(dir('.'), devtools::as.package)

  ###
  mat <- build_pkgs_matrix(pkgs)
  expect_true(is.matrix(mat) && is.character(mat))

  deps <- mat[, c('Package', 'Depends', 'Imports', 'Suggests', 'LinkingTo')]

  .col <- function(idx) unname(deps[, idx])
  expect_identical(.col('Package'), paste0(LETTERS[1:4], LETTERS[1:4]))
  expect_identical(.col('Depends'), c("BB", "CC", "", ""))
  expect_identical(.col('Imports'), c("", "CC", "", ""))
  expect_identical(.col('Suggests'), c("", "", "DD", ""))
})


test_that("compute_pkgs_dependencies_graph", {
  dir <- setup_temp_dir(setwd = FALSE)

  .sort_graph <- function(g) {
    top <- igraph::topo_sort(g)
    nodes <- igraph::V(g)$name
    nodes[top]
  }
  .create_pkg <- function(name, imports) {  pkg_create(dir, name, imports = imports)  }
  are_adjacent <- igraph::are_adjacent

  pkga <- .create_pkg('AA', imports = 'stats')

  ### edge cases
  expect_error(compute_pkgs_dependencies_graph(list()), 'bad arg')

  ### A: no deps inside lib
  g <- compute_pkgs_dependencies_graph(list(pkga))

  expect_s3_class(g, 'igraph')
  expect_true(igraph::is_directed(g))
  expect_identical(.sort_graph(g), 'AA')
  
  ### B: B->A
  pkgb <- .create_pkg('BB', imports = c('plyr', 'AA'))

  g <- compute_pkgs_dependencies_graph(list(pkga, pkgb))

  expect_equal(.sort_graph(g), c('BB', 'AA'))
  expect_true(are_adjacent(g, 'BB', 'AA'))

  ### C: C->B->A
  pkgc <- .create_pkg('CC', imports = c('BB'))

  g <- compute_pkgs_dependencies_graph(list(pkga, pkgb, pkgc))
  
  expect_equal(.sort_graph(g), c('CC', 'BB', 'AA'))
  expect_true(are_adjacent(g, 'CC', 'BB'))

  ### D: D->C->B->A and D->B
  pkgd <- .create_pkg('DD', imports = c('CC', 'BB'))
  
  g <- compute_pkgs_dependencies_graph(list(pkga, pkgb, pkgc, pkgd))

  expect_equal(.sort_graph(g), c('DD', 'CC', 'BB', 'AA'))
  expect_true(are_adjacent(g, 'DD', 'CC'))
  expect_true(are_adjacent(g, 'DD', 'BB'))

  ### E: E->A
  pkge <- .create_pkg('EE', imports ='AA')

  g <- compute_pkgs_dependencies_graph(list(pkga, pkgb, pkgc, pkgd, pkge))
  
  expect_true(are_adjacent(g, 'EE', 'AA'))
  sg <- .sort_graph(g)
  expect_true(sg[1] == 'DD' && sg[length(sg)] == 'AA')


  ### create a cycle of size 2 ##################################
  cycle1 <- .create_pkg('C1', imports = 'C2')
  cycle2 <- .create_pkg('C2', imports = 'C1')

  g <- compute_pkgs_dependencies_graph(list(cycle1, cycle2))
  
  expect_true(are_adjacent(g, 'C1', 'C2') &&  are_adjacent(g, 'C2', 'C1'))
  expect_false(igraph::is_dag(g))
})

# N.B: already tested via compute_pkgs_dependencies_graph
test_that("build_pkgs_dependency_graph", {
  # edge case
  g <- build_pkgs_dependency_graph(list())
  expect_equal(igraph::vcount(g), 0)
})