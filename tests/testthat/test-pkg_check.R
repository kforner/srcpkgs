

.pkg_check <- 
test_that("pkg_check", {
  testthat::skip_on_cran()

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
  df <- summary(chk)
  
  expect_equal(nrow(df), 1)
  expect_type(df$time, "double")

  df$time <- 1
  expect_identical(df, data.frame(package = "BB", errors = 1L, warnings = 0L, notes = 0L, time = 1))

  # as.data.frame - N.B: currently same as summary()
  expect_identical(as.data.frame(chk), summary(chk))

  # as.logical
  expect_false(as.logical(chk))

  # BB should error if e.g. error_on=="error"
  capture_output(expect_error(pkg_check("BB", src_pkgs = src_pkgs, error_on = "error", quiet = TRUE), 
    "R CMD check found ERRORs"))

  ### print
  # local_reproducible_output(crayon = TRUE, unicode = TRUE)
  # N.B: since the print output includes a stacktrace, which depends on testthat/rlang versions
  # we can not expect_snapshot()
  # anyway the print method is provided by the rcmdcheck package/object
  
  mute(expect_error(print(chk), NA))
#   # fix time to get a reproducible output
#   chk$duration <- 1.5
#   # fix non deterministic path to get a reproducible output
#   chk$test_fail[[1]] <- sub(".*setup.R", "in path /dummy/setup.R", chk$test_fail[[1]])
#   expect_snapshot(print(chk))
})



