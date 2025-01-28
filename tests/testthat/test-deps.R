.pkgs_deps <- 
test_that("pkg_deps", {
  # A->B->C->D, B->D->E, Z
  examples_srcpkgs_complex_deps()

  pkgs <- find_srcpkgs('.')

  NADA <- character()

  ### CC
  # N.B: the order matters (topological)
  expect_identical(pkg_deps('CC', pkgs), c("DD", "EE"))
  expect_identical(pkg_deps('CC', pkgs, source = FALSE), NADA)
  expect_identical(pkg_deps('CC', pkgs, suggests = FALSE), c("DD", "EE"))
  expect_identical(pkg_deps('CC', pkgs, depends = FALSE), c("DD", "EE"))
  expect_identical(pkg_deps('CC', pkgs, depends = FALSE, suggests = FALSE), "DD")
  
  ### A
  expect_identical(pkg_deps('AA', pkgs), c("BB", "stats", "CC", "DD", "EE"))
  expect_identical(pkg_deps('AA', pkgs, installed = FALSE), c("BB", "CC", "DD", "EE"))
  expect_identical(pkg_deps('AA', pkgs, source = FALSE), "stats")
  expect_identical(pkg_deps('AA', pkgs, imports = FALSE), c("CC", "EE"))
  expect_identical(pkg_deps('AA', pkgs, imports = FALSE, depends = FALSE), NADA)

  ## E
  expect_identical(pkg_deps('EE', pkgs), NADA)

  ### reverse
  expect_identical(pkg_deps('EE', pkgs, reverse = TRUE), c("DD", "CC", "BB", "AA"))
  expect_identical(pkg_deps('AA', pkgs, reverse = TRUE), NADA)

  ### external/installed package
  expect_identical(pkg_deps('stats', pkgs), NADA)
  expect_identical(pkg_deps('stats', pkgs, reverse = TRUE), "AA")
})



.get_srcpkg_dependencies <- 
test_that("get_srcpkg_dependencies", {
  setup_temp_dir()
  
  ### pkg with all types of deps
  pkg <- pkg_create('.', 'AA', imports = c('i1', 'i2'), depends = 'd1', suggests = c('s1', 's2'))
  deps <- get_srcpkg_dependencies(pkg)
  expect_identical(deps, list(imports = c("i1", "i2"), depends = "d1", suggests = c("s1", "s2")))

  ### package with no deps
  pkg <- pkg_create('.', 'nodeps')
  deps <- get_srcpkg_dependencies(pkg)
  expect_identical(deps, list(imports = NULL, depends = NULL, suggests = NULL))
})

