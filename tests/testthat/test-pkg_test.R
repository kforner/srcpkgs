.pkg_test_errors <- 
test_that(".pkg_test_errors", {
  src_pkgs <- examples_srcpkgs_basic()
  add_dummy_test_to_srcpkgs(src_pkgs)

  ### an exception in the setup
  pkga <- src_pkgs$AA
  writeLines(r"{ stop("aie aie aie") }", file.path(pkga$path, "tests/testthat/setup.R"))
 
  expect_error(pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent"), "aie aie aie")
})

.pkg_test <- 
test_that("pkg_test", {
  ### trivial example A->B
  src_pkgs <- examples_srcpkgs_basic()

  ### pkg with no tests
  is_empty_pkg_test <- function(x) inherits(x, "pkg_test") && is.list(x) && length(x) == 0

  res <- pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent")
  expect_true(is_empty_pkg_test(res))
  expect_true(is_empty_pkg_test(pkg_test('BB', src_pkgs = src_pkgs, reporter = "silent")))

  # as.data.frame
  df <- as.data.frame(res)

  expect_identical(names(df), c("file", "test", "nb", "failed", "passed", "skipped", "error", "warning", "time"))
  expect_equal(nrow(df) ,0)

  # no tests --> no failure
  expect_true(as.logical(res))
  expect_match(capture.output(print(res), type = "message"), "package AA has no tests", fixed = TRUE, all = FALSE)

  sdf <- summary(res)
  expect_identical(names(sdf), c("file", "nb", "failed", "passed", "skipped", "error", "warning", "time"))
  expect_equal(nrow(sdf) ,0)

  ###### with tests
  add_dummy_test_to_srcpkgs(src_pkgs)
  ### one package with a dep

  resAA <- res <- pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent")

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

  ### if loaders_hack_enabled
  if (! is_loaders_hack_enabled()) {
    hack_r_loaders()
    on.exit(unhack_r_loaders(), add = TRUE)
  }
  res <- pkg_test('AA', src_pkgs = src_pkgs, reporter = "silent")
  expect_identical(as.data.frame(fix_test_result_timings(res)), as.data.frame(fix_test_result_timings(resAA)))
})



.pkg_test_s3_methods <- 
test_that("pkg_test_s3_methods", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

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

  ### as.logical
  expect_false(as.logical(res))

  # keep only successful tests
  good <- res[df$failed == 0 & !df$error]
  class(good) <- class(res)

  expect_true(as.logical(good))

  ### print
  withr::local_options(list(cli.num_colors = 256))
  expect_snapshot(print(fix_test_result_timings(res)))
})


