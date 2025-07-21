

.pkg_check <- 
test_that("pkg_check", {
  src_pkgs <- examples_srcpkgs_basic()
  setup_temp_dir()

  ###### with tests
  add_dummy_test_to_srcpkgs(src_pkgs)
  # make BB to fail
  writeLines(r"{ stop("aie aie aie") }", file.path(src_pkgs$BB$path, "tests/testthat/setup.R"))

  ### error_on = "never"
  expect_error(chk <- pkg_check("BB", src_pkgs = src_pkgs, error_on = "never", quiet = TRUE), NA)

  expect_s3_class(chk, "pkg_check")
  # s3 generic: summary
  expect_identical(summary(chk), data.frame(package = "BB", errors = 1L, warnings = 0L, notes = 0L))

  # BB should error if e.g. error_on=="error"
  capture_output(expect_error(chk <- pkg_check("BB", src_pkgs = src_pkgs, error_on = "error", quiet = TRUE)
    , "R CMD check found ERRORs"))
})



