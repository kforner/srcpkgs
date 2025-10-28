

.dummy_srcpkg_path <- 
test_that("dummy_srcpkg_path", { 
  expect_true(dir.exists(dummy_srcpkg_path()))
})


.setup_and_get_dummy_srcpkg <- 
test_that("setup_and_get_dummy_srcpkg", {

  pkg <- setup_and_get_dummy_srcpkg()
  on.exit(unlink(dirname(pkg$path), recursive = TRUE), add = TRUE)

  expect_s3_class(pkg, "srcpkg")
  expect_identical(pkg$package, "dummy.srcpkg")

  ## what if the directory and package already exist
  pkg2 <- setup_and_get_dummy_srcpkg(dirname(pkg$path))
  expect_identical(pkg2, pkg)

  ## testing
  srcpkgs <- srcpkgs(list(pkg))
  res <- pkg_test(pkg, src_pkgs = srcpkgs, reporter = "silent")

  expect_s3_class(res, "pkg_test")
  df <- summary(res)

  expect_setequal(df$failed, c(1, 0))

  ## checking
  chk <- pkg_check(pkg, src_pkgs = srcpkgs, error_on = "never", quiet = TRUE)

  expect_s3_class(chk, "pkg_check")
})
