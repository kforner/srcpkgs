test_that("build_pkgs_matrix", {
  build_pkgs_matrix <- srcpkgs:::build_pkgs_matrix
  setup_temp_dir()

  pkg_create('.', 'AA', depends = 'BB')
  pkg_create('.', 'BB', imports = 'CC', depends = 'CC')
  pkg_create('.', 'CC', suggests = 'DD')
  pkg_create('.', 'DD')
  pkgs <- lapply(dir('.'), devtools::as.package, create = FALSE)

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


# test_that("build_pkgs_dependency_graph works", {
#   build_pkgs_dependency_graph <- srcpkgs:::build_pkgs_dependency_graph
#   dir <- setup_temp_dir(chdir = FALSE)

#   .sort_graph <- function(g) {
#     top <- igraph::topological.sort(g)
#     nodes <- igraph::V(g)$name
#     nodes[top]
#   }
#   are.connected <- igraph::are.connected

#   .create_pkg <- function(name, imports) {
#     create_package(dir, name, imports = imports)
#   }
#   .src_pkgs <- function() {
#     ##as_pkgs(find_src_pkgs(dir))
#     find_srcpkgs(dir) # do not need to find qbr
#   }

#   deps_list <- function(x) {
#     scrpkgs:::compute_srcpkgs_dependencies(x, src_pkgs = .src_pkgs())
#   }

#   pkga <- .create_pkg('AA', imports = 'stats')

#   ### edge cases
#   expect_error(build_pkgs_deps_graph(NULL), 'give a list')

#   g <- build_pkgs_deps_graph(list())
#   expect_is(g, 'igraph')

#   ### A: no deps inside lib
#   deps <- deps_list(scrpkgs:::as_srcpkgs(pkga))


#   g <- build_pkgs_deps_graph(deps)
#   expect_is(g, 'igraph')
#   expect_true(igraph::is.directed(g))
#   ### B: B->A
#   pkgb <- .create_pkg('BB', imports = c('plyr', 'AA'))

#   g <- build_pkgs_deps_graph(deps_list(.src_pkgs()))

#   expect_equal(.sort_graph(g), c('BB', 'AA'))
#   expect_true(are.connected(g, 'BB', 'AA'))

#   ### C: C->B->A
#   .create_pkg('CC', imports = c('BB'))
#   g <- build_pkgs_deps_graph(deps_list(.src_pkgs()))
#   expect_equal(.sort_graph(g), c('CC', 'BB', 'AA'))
#   expect_true(are.connected(g, 'CC', 'BB'))

#   ### D: D->C->B->A and D->B
#   .create_pkg('DD', imports = c('CC', 'BB'))
#   g <- build_pkgs_deps_graph(deps_list(.src_pkgs()))
#   expect_equal(.sort_graph(g), c('DD', 'CC', 'BB', 'AA'))
#   expect_true(are.connected(g, 'DD', 'CC'))
#   expect_true(are.connected(g, 'DD', 'BB'))

#   ### E: E->A
#   .create_pkg('EE', imports ='AA')
#   g <- build_pkgs_deps_graph(deps_list(.src_pkgs()))
#   expect_true(are.connected(g, 'EE', 'AA'))
#   sg <- .sort_graph(g)
#   expect_true(sg[1] == 'DD' && sg[length(sg)] == 'AA')

#   ### create a cycle of size 2
#   .create_pkg('C1', imports = 'C2')
#   .create_pkg('C2', imports = 'C1')
#   g <- build_pkgs_deps_graph(deps_list(.src_pkgs()))
#   expect_true(are.connected(g, 'C1', 'C2') &&  are.connected(g, 'C2', 'C1'))
#   expect_false(igraph::is.dag(g))
# })

