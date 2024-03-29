MINIMAL_PATH <- 'test_library_srcpkgs/minimal'
MINIMAL <- devtools::as.package(MINIMAL_PATH)
srcpkg <- srcpkgs:::srcpkg

test_that("srcpkg", {
  ###
  srcpkg <- srcpkg(MINIMAL)
  expect_s3_class(srcpkg, 'srcpkg')
  expect_true(devtools::is.package(srcpkg))
  expect_true(is.na(srcpkg$MD5))

  # using md5 param
  srcpkg <- srcpkg(MINIMAL, md5 = 'toto')
  expect_s3_class(srcpkg, 'srcpkg')
  expect_identical(srcpkg$MD5, 'toto')

  # works also directly on a srcpkg
  srcpkg <- srcpkg(MINIMAL, md5 = 'toto')
  srcpkg2 <- srcpkg(srcpkg)
  expect_identical(srcpkg2, srcpkg)

  # with a path
  srcpkg <- srcpkg(MINIMAL)
  srcpkg2 <- srcpkg(path = MINIMAL_PATH)
  expect_identical(srcpkg2, srcpkg)
})


test_that("print.srcpkg", {
  srcpkg <- srcpkg(MINIMAL)
  out <- capture.output(print(srcpkg), type = 'message')
  expect_match(out, "minimal")
})

test_that("as_pkg_name", {
  as_pkg_name <- srcpkgs:::as_pkg_name

  # from name
  expect_identical(as_pkg_name('AA'), 'AA')

  # from devtools package
  expect_identical(as_pkg_name(MINIMAL), MINIMAL$package)

  # from srcpkg
  srcpkg <- srcpkg(MINIMAL)
  expect_identical(as_pkg_name(srcpkg), srcpkg$package)

  # from something else
  expect_error(as_pkg_name(1),'bad arg')
})
