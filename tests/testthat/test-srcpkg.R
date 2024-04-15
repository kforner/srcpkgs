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


test_that("as_srcpkg", {
  ### empty
  expect_error(as_srcpkg(NULL), 'empty')
  expect_error(as_srcpkg(character()), 'empty')

  expect_error(as_srcpkg(1), 'bad arg')

  ### srcpkg
  srcpkg <- srcpkg(MINIMAL)
  expect_identical(as_srcpkg(srcpkg), srcpkg)

  ### "package" object
  expect_identical(as_srcpkg(MINIMAL), srcpkg)

  ### path
  expect_identical(as_srcpkg(MINIMAL_PATH), srcpkg)

  ### name is not currently supported
  expect_error(as_srcpkg(MINIMAL$package), 'does not exist')
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
