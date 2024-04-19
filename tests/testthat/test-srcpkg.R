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

  ### srcpkg name
  src_pkgs <- srcpkgs(list(srcpkg))
  expect_identical(as_srcpkg('minimal', src_pkgs = src_pkgs), srcpkg)

  ### path
  expect_identical(as_srcpkg(MINIMAL_PATH), srcpkg)
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


test_that("get_srcpkg_dependencies", {
  setup_temp_dir()
  
  ### pkg with all types of deps
  pkg <- pkg_create('.', 'AA', imports = c('i1', 'i2'), depends = 'd1', suggests = c('s1', 's2'))
  deps <- get_srcpkg_dependencies(pkg)
  expect_identical(deps, list(imports = c("i1", "i2"), depends = "d1", suggests = c("s1", "s2")))

  ### package with no deps
  pkg <- pkg_create('.', 'nodeps')
  deps <- get_srcpkg_dependencies(pkg)
  expect_identical(deps, list(imports = NULL, depends = NULL, suggests = NULL))
})




