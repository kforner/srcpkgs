
.pkgs_install <- 
test_that(".pkgs_install", {
  # G-->C-->B-->A--->stats, F-->D-->B, E-->A, Z, G--->utils
  src_pkgs <- examples_srcpkgs_complex_imports()
  setup_temp_dir()
  LIB <- 'lib'

  ### a package with deps
  res <- pkgs_install("GG", LIB, src_pkgs = src_pkgs, quiet = TRUE)
  
  expect_identical(res, c("AA", "BB", "CC", "GG"))
  expect_equal(installed.packages(LIB)[, "Package"], res, ignore_attr = TRUE)

  # again
  expect_null( pkgs_install("GG", LIB, src_pkgs = src_pkgs, quiet = TRUE) )

  ### a package with some deps already installed
  res <- pkgs_install("FF", LIB, src_pkgs = src_pkgs, quiet = TRUE)
  expect_identical(res, c("DD", "FF"))

  # only deps
  expect_null( pkgs_install("EE", LIB, src_pkgs = src_pkgs, only_deps = TRUE, quiet = TRUE) )

  ### multiple packages
  res <- pkgs_install(names(src_pkgs), LIB, src_pkgs = src_pkgs, quiet = TRUE)
  expect_identical(res, c("EE", "ZZ"))
})


.pkg_install_nodeps <- 
test_that("pkg_install_nodeps", {
  # A->B->C->D, B->D->E, Z
  src_pkgs <- examples_srcpkgs_complex_deps()
  setup_temp_dir()
  LIB <- 'toto'

  expect_error(pkg_install_nodeps(src_pkgs[[1]]$path, LIB), "does not exist")

  dir.create(LIB)

   # no NAMESPACE file yet
  expect_error(pkg_install_nodeps(src_pkgs$ZZ$path, LIB, quiet = TRUE), "failed")
  ## pkg_roxygenise --> generate the NAMESPACE
  pkg_roxygenise(src_pkgs$ZZ$path, quiet = TRUE)
  pkg_unload(src_pkgs$ZZ, quiet = TRUE)
  
  md5 <- pkg_install_nodeps(src_pkgs$ZZ$path, LIB, quiet = TRUE)
  
  expect_identical(installed.packages(LIB)[, "Package"], "ZZ")
  expect_identical(md5, pkg_md5sum(src_pkgs$ZZ$path))  
  expect_true(dir.exists(file.path(LIB, "ZZ")))

  expect_false(pkg_needs_install(src_pkgs$ZZ$path, LIB))

  ## modify the source package
  writeLines("# sorry", file.path(src_pkgs$ZZ$path, "R/toto.R"))
  expect_true(pkg_needs_install(src_pkgs$ZZ$path, LIB))
})





