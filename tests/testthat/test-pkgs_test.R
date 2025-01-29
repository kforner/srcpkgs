

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

  res1 <- res[1]; class(res1) <- class(res)
  res2 <- res[2];class(res2) <- class(res)

  ####### =============== S3 methods ================================
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
  expect_snapshot(print(fix_pkg_tests_results__timings(res)))
})



