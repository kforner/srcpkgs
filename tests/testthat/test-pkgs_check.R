

.pkgs_check <- 
test_that("pkgs_check", {
  src_pkgs <- examples_srcpkgs_basic()
  setup_temp_dir()

  ###################################### edge cases ###############################
  expect_error(pkgs_check(NULL), 'No package to test')

  ###### with tests
  # add_dummy_test_to_srcpkgs(src_pkgs)
  add_dummy_test_to_srcpkg(src_pkgs[[1]], with_errors = FALSE, with_failures = FALSE)
  add_dummy_test_to_srcpkg(src_pkgs[[2]])

  # make BB to fail
  writeLines(r"{ stop("aie aie aie") }", file.path(src_pkgs$BB$path, "tests/testthat/setup.R"))

  ### 
  mute(chks <- pkgs_check(src_pkgs = src_pkgs, quiet = FALSE))

  expect_s3_class(chks, "pkgs_check")
  expect_true(is.list(chks))
  expect_length(chks, 2)
  expect_s3_class(chks[[1]], "pkg_check")
  expect_s3_class(chks[[2]], "pkg_check")

  ####### =============== S3 methods ================================
  ### as.data.frame - one row per package
  df <- as.data.frame(chks)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 2)

  for (i in 1:2) {
    expected <- summary(chks[[i]])
    rownames(expected) <- src_pkgs[[i]]$package
    expect_identical(df[i,], expected)
  }

  ### summary
  sdf <- summary(chks)

  expect_equal(nrow(sdf), 1)
  df$package <- 1
  df2 <- df[1,] + df[2, ]
  rownames(df2) <- NULL
  expect_equal(sdf, df2)
  
  ### as.logical
  expect_false(as.logical(chks))

  #### fail_on_error
  expect_error(chks <- pkgs_check(src_pkgs = src_pkgs, quiet = TRUE, fail_on_error = TRUE), "checks failed")

  ### print
  # fix durations for reproducible output
  for (i in seq_along(chks)) chks[[i]]$duration <- pi

  # create a dummy successful pkgs_check by replicating chks[[1]]
  chks_ok <- new_pkgs_check(list(chks[[1]], chks[[1]]), names(chks))
  expect_true(as.logical(chks_ok))

  # call the print method since there seems to be a bug with expect_snapshot() and covr
  mute(expect_error(print(chks), NA))
  mute(expect_error(print(chks_ok), NA))

  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  expect_snapshot(print(chks))
  expect_snapshot(print(chks_ok))
})



