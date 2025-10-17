
.pkgs_test_when_no_tests <- 
test_that("pkgs_test_when_no_tests", {
  src_pkgs <- examples_srcpkgs_basic()

  ### no tests at all
  res <- pkgs_test(src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)

  expect_s3_class(res, "pkgs_test")
  expect_identical(names(res), c("AA", "BB"))

  ## all empty since no tests
  expect_s3_class(res$AA, "pkg_test")
  expect_s3_class(res$BB, "pkg_test")
  expect_length(res$AA, 0)
  expect_length(res$BB, 0)

  # as.data.frame
  df <- as.data.frame(res)

  expect_identical(df$package, c("AA", "BB"))
  expect_true(all(df[, -1] == 0))

  # as.logical
  expect_true(as.logical(res))

  # summary
  sdf <- summary(res)
  expect_equal(sdf$package, 2)
  expect_true(all(sdf[, -1] == 0))

  # print
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  expect_snapshot(print(res))

  ### only BB has tests
  add_dummy_test_to_srcpkg(src_pkgs$BB)
  
  res <- pkgs_test(src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)
  expect_s3_class(res, "pkgs_test")

  # as.data.frame
  df <- as.data.frame(res)

  expect_identical(df$package, c("AA", "BB"))
  expect_true(all(df[1, -1] == 0))
  expect_equal(df["BB", "nb"], 17)

  # as.logical
  expect_false(as.logical(res))

  # summary
  sdf <- summary(res)
  expect_equal(sdf$package, 2)
  expect_equal(sdf[, -1], df[2, -1], ignore_attr = TRUE)

  # print
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  expect_snapshot(print(fix_pkg_tests_results_timings(res)))
})


.pkgs_test <- 
test_that("pkgs_test", {
  src_pkgs <- examples_srcpkgs_basic()


  ###### with tests
  add_dummy_test_to_srcpkgs(src_pkgs)
  # make BB to fail
  writeLines(r"{ stop("aie aie aie") }", file.path(src_pkgs$BB$path, "tests/testthat/setup.R"))

  ### 
  res <- pkgs_test(src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)

  expect_s3_class(res, "pkgs_test")
  expect_true(is.list(res))
  expect_length(res, 2)
  expect_s3_class(res[[1]], "pkg_test")
  expect_s3_class(res[[2]], "try-error")

  res1 <- subset_s3_list(res, 1)
  res2 <- subset_s3_list(res, 2)

  ## filter
  subres <- pkgs_test(src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE, filter = "A")
  expect_identical(names(subres), "AA")

  ## alternate inputs
  # names
  res2 <- pkgs_test("AA", src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)

  .cmp_res <- function(res1, res2) {
    df1 <- as.data.frame(res1)
    df2 <- as.data.frame(res2)
    df1$time <- df2$time <- NULL
    identical(df1, df2)
  }
  expect_true(.cmp_res(res2, subset_s3_list(res, "AA")))
  
  # srcpkgs objects
  resbis <- pkgs_test(src_pkgs, src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)
  expect_identical(names(resbis), c("AA", "BB"))
  
  res22 <- pkgs_test(subset_s3_list(src_pkgs, "BB"), src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)
  expect_identical(names(res22), "BB")

  ####### =============== S3 methods ================================
  res <- pkgs_test(src_pkgs = src_pkgs, reporter = "silent", quiet = TRUE)
  res1 <- subset_s3_list(res, 1)
  res2 <- subset_s3_list(res, 2)

  ### as.data.frame
  df <- as.data.frame(res)

  expect_true(is.data.frame(df))
  expect_true(all(c("package", "nb", "failed", "passed", "skipped", "error", "warning", "time") %in% names(df)))
  expect_identical(df$package, names(src_pkgs))

  expect_equal(df[1, "nb"], sum(as.data.frame(res[[1]])$nb))
  # 2nd pkg BB has crashed
  expect_equal(df[2, "error"], 1)
  other_cols <- setdiff(names(df), c("package", "error"))
  expect_true(all(is.na(df[2, other_cols, drop = TRUE])))
  # 1 pkg no error

  df1 <- as.data.frame(res1)
  df1$package <- NULL
  rownames(df1) <- NULL
  expect_identical(df1, summary(res1[[1]], col = NULL))
  
  # 1 pkg with error
  df2 <- as.data.frame(res2)
  df2$skipped <- as.integer(df2$skipped)
  expect_identical(df2, df[2,])

  ### as.logical
  expect_false(as.logical(res))
  expect_false(as.logical(res1))
  expect_false(as.logical(res2))

  # keep only successful tests
  res11_good <- res1[[1]]
  df11 <- as.data.frame(res11_good)
  res11_good <- res11_good[df11$failed == 0 & !df11$error]
  class(res11_good) <- class(res1[[1]])
  good <- list(AA = res11_good);  class(good) <- class(res)

  expect_true(as.logical(good))

  ### summary
  sdf <- summary(res)
  expect_true(is.data.frame(sdf))
  expect_equal(nrow(sdf), 1)
  
  expected <- df[1,]
  expected$error <- expected$error + 1
  rownames(expected) <- NULL
  expected$package <- 2
  expect_identical(sdf, expected)

  ### print
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  expect_snapshot(print(fix_pkg_tests_results_timings(res)))
})



