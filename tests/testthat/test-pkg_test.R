.pkg_test <- 
test_that("pkg_test", {
  ### trivial example A->B
  src_pkgs <- examples_srcpkgs_basic()

  ### pkg with no tests
  expect_null(pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent"))
  expect_null(pkg_test('BB', src_pkgs = src_pkgs, reporter = "silent"))

  ###### with tests
  add_dummy_test_to_srcpkgs(src_pkgs)
  ### one package with a dep

  res <- pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent")

  expect_s3_class(res, "pkg_test")
  df <- as.data.frame(res)
  expect_equal(sum(df$failed), 6)
  pkg <- attr(res, 'pkg')
  expect_identical(pkg, src_pkgs$AA)

  ### pkg with no deps
  res <- pkg_test('BB', src_pkgs = src_pkgs, reporter = "silent")
  expect_s3_class(res, "pkg_test")

  ####### filter
  res <- pkg_test('AA', filter = "success", src_pkgs = src_pkgs, reporter = "silent")
  df <- as.data.frame(res)
  expect_equal(sum(df$failed), 0)

  res <- pkg_test('AA', filter = "failure", src_pkgs = src_pkgs, reporter = "silent")
  df <- as.data.frame(res)
  expect_true(all(df$failed > 0))
})



.pkg_test_s3_methods <- 
test_that("pkg_test_s3_methods", {
  local_reproducible_output(crayon = TRUE)

  src_pkgs <- examples_srcpkgs_complex_deps()
  add_dummy_test_to_srcpkgs(src_pkgs)

  res <- pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent")

  ### as.data.frame
  # fetch original data frame from testthat
  tr <- res
  class(tr) <- class(res)[-1]
  df0 <- as.data.frame(tr)

  df <- as.data.frame(res)

  # agree on common cols
  common_cols <- setdiff(intersect(names(df0), names(df)), "file")
  expect_identical(df0[common_cols], df[common_cols])

  # extra cols
  extra_cols <- setdiff(names(df), names(df0))
  expect_true("time" %in% extra_cols)

  ### print
  withr::local_options(list(cli.num_colors = 256))
  # N.B: because of timings, the execution is not reproducible, so
  # we can not use expect_snapshot
  capture_output(expect_error(print(res), NA))
  
  ### summary
  df <- as.data.frame(res)
  # by file (default)
  sdf <- summary(res)
  expect_setequal(names(sdf), setdiff(names(df), "test"))

  expect_equal(anyDuplicated(sdf$file), 0)
  expect_equal(sum(sdf$nb), sum(df$nb))
  expect_equal(sum(sdf$failed), sum(df$failed))

  # overview: col = NULL
  sdf <- summary(res, col = NULL)
  
  expect_equal(nrow(sdf), 1)
  expect_setequal(names(sdf), setdiff(names(df), c("test", "file")))
  expect_equal(sum(sdf$nb), sum(df$nb))
  expect_equal(sum(sdf$failed), sum(df$failed))
})


.pkgs_test <- 
test_that("pkgs_test", {
  ### trivial example A->B
  src_pkgs <- examples_srcpkgs_basic()

  # ### pkg with no tests
  # expect_null(pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent"))
  # expect_null(pkg_test('BB', src_pkgs = src_pkgs, reporter = "silent"))

  ###### with tests
  add_dummy_test_to_srcpkgs(src_pkgs)
  ### 
  res <- pkgs_test(src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)

  expect_s3_class(res, "pkgs_test")
  expect_true(is.list(res))
  expect_length(res, 2)
  expect_s3_class(res[[1]], "pkg_test")
  
  # browser()


})



