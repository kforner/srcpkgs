.pkgs_deps <- 
test_that("pkgs_deps", {
  # G-->C-->B-->A--->stats, F-->D-->B, E-->A, Z, G--->utils
  pkgs <- examples_srcpkgs_complex_imports()

  expect_identical(pkgs_deps("CC", pkgs), c("BB", "AA", "stats"))
  expect_identical(pkgs_deps("FF", pkgs), c("DD", "BB", "AA", "stats"))
  expect_identical(pkgs_deps(c("FF", "CC"), pkgs), c("DD", "BB", "AA", "stats"))

  expect_identical(pkgs_deps("GG", pkgs), c("CC", "utils", "BB", "AA", "stats"))
  expect_identical(pkgs_deps(c("FF", "GG"), pkgs), c("CC", "DD", "utils", "BB", "AA", "stats"))

  expect_identical(pkgs_deps(c("FF", "GG"), pkgs, imports = FALSE), "utils")
  expect_identical(pkgs_deps(c("FF", "GG"), pkgs, depends = FALSE), c("CC", "DD", "BB", "AA", "stats"))
  expect_identical(pkgs_deps(c("FF", "GG"), pkgs, installed = FALSE), c("CC", "DD", "BB", "AA"))
  expect_identical(pkgs_deps(c("FF", "GG"), pkgs, source = FALSE), c("stats", "utils"))

  ### reverse
  expect_identical(pkgs_deps("AA", pkgs, reverse = TRUE), c("BB", "DD", "CC", "GG", "FF", "EE"))

  expect_identical(pkgs_deps("CC", pkgs, reverse = TRUE), "GG")
  expect_identical(pkgs_deps("DD", pkgs, reverse = TRUE), "FF")
  expect_identical(pkgs_deps(c("CC", "DD"), pkgs, reverse = TRUE), c("GG", "FF"))
})



.pkgs_deps_1pkg <- 
test_that(".pkgs_deps_1pkg", {
  # A->B->C->D, B->D->E, Z
  examples_srcpkgs_complex_deps()

  pkgs <- find_srcpkgs('.')

  NADA <- character()

  ### CC
  # N.B: the order matters (topological)
  expect_identical(pkgs_deps('CC', pkgs), c("DD", "EE"))
  expect_identical(pkgs_deps('CC', pkgs, source = FALSE), NADA)
  expect_identical(pkgs_deps('CC', pkgs, suggests = FALSE), c("DD", "EE"))
  expect_identical(pkgs_deps('CC', pkgs, depends = FALSE), c("DD", "EE"))
  expect_identical(pkgs_deps('CC', pkgs, depends = FALSE, suggests = FALSE), "DD")
  
  ### A
  expect_identical(pkgs_deps('AA', pkgs), c("BB", "stats", "CC", "DD", "EE"))
  expect_identical(pkgs_deps('AA', pkgs, installed = FALSE), c("BB", "CC", "DD", "EE"))
  expect_identical(pkgs_deps('AA', pkgs, source = FALSE), "stats")
  expect_identical(pkgs_deps('AA', pkgs, imports = FALSE), c("CC", "EE"))
  expect_identical(pkgs_deps('AA', pkgs, imports = FALSE, depends = FALSE), NADA)

  ## E
  expect_identical(pkgs_deps('EE', pkgs), NADA)

  ### reverse
  expect_identical(pkgs_deps('EE', pkgs, reverse = TRUE), c("DD", "CC", "BB", "AA"))
  expect_identical(pkgs_deps('AA', pkgs, reverse = TRUE), NADA)

  ### external/installed package
  expect_identical(pkgs_deps('stats', pkgs), NADA)
  expect_identical(pkgs_deps('stats', pkgs, reverse = TRUE), "AA")
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

