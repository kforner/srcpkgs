
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


