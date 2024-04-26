test_that("srcpkg", {
  setup_temp_dir()
  srcpkg <- pkg_create('.', 'minimal')
  ###
  expect_s3_class(srcpkg, 'srcpkg')
  expect_true(devtools::is.package(srcpkg))

  # works also directly on a srcpkg
  srcpkg2 <- srcpkg(srcpkg)
  expect_identical(srcpkg2, srcpkg)

  # with a path
  srcpkg2 <- srcpkg(path = srcpkg$path)
  expect_identical(srcpkg2, srcpkg)
})


test_that("as_srcpkg", {
  setup_temp_dir()
  srcpkg <- pkg_create('.', 'minimal')

  ### empty
  expect_error(as_srcpkg(NULL), 'empty')
  expect_error(as_srcpkg(character()), 'empty')

  expect_error(as_srcpkg(1), 'bad arg')

  ### srcpkg
  expect_identical(as_srcpkg(srcpkg), srcpkg)

  ### "package" object
  pkg <- devtools::as.package(srcpkg$path)
  expect_identical(as_srcpkg(pkg), srcpkg)

  ### srcpkg name
  src_pkgs <- srcpkgs(list(srcpkg))
  expect_identical(as_srcpkg('minimal', src_pkgs = src_pkgs), srcpkg)

  ### path
  expect_identical(as_srcpkg(srcpkg$path, src_pkgs = src_pkgs), srcpkg)
})

test_that("print.srcpkg", {
  setup_temp_dir()
  srcpkg <- pkg_create('.', 'minimal')

  out <- capture.output(print(srcpkg), type = 'message')
  expect_match(out, "minimal")
})

test_that("as_pkg_name", {
  setup_temp_dir()
  srcpkg <- pkg_create('.', 'minimal')

  # from name
  expect_identical(as_pkg_name('AA'), 'AA')

  # from devtools package
  pkg <- devtools::as.package(srcpkg$path)
  expect_identical(as_pkg_name(pkg), srcpkg$package)

  # from srcpkg
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




