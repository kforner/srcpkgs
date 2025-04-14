

.pkgs_check <- 
test_that("pkgs_check", {
  src_pkgs <- examples_srcpkgs_basic()
  setup_temp_dir()

  ###### with tests
  add_dummy_test_to_srcpkgs(src_pkgs)
  # make BB to fail
  writeLines(r"{ stop("aie aie aie") }", file.path(src_pkgs$BB$path, "tests/testthat/setup.R"))

  ### 
  chks <- pkgs_check(src_pkgs = src_pkgs, quiet = TRUE)

  expect_s3_class(chks, "pkgs_check")
  expect_true(is.list(chks))
  expect_length(chks, 2)
  expect_s3_class(chks[[1]], "pkg_check")
  expect_s3_class(chks[[2]], "pkg_check")


  ####### =============== S3 methods ================================
  ### summary
  sdf <- summary(chks)
  expect_true(is.data.frame(sdf))
  expect_equal(nrow(sdf), 2)

  for (i in 1:2) {
    expected <- summary(chks[[i]])
    rownames(expected) <- src_pkgs[[i]]$package
    expect_identical(sdf[i,], expected)
  }

  ### print
  expect_snapshot(print(chks))

  #### fail_on_error
  expect_error(chks <- pkgs_check(src_pkgs = src_pkgs, quiet = TRUE, fail_on_error = TRUE), "checks failed")

})



