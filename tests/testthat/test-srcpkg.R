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

  ### with dependencies
  pkg <- pkg_create('.', 'withdeps', imports = c("i1", "i2"), depends = "d1", suggests = c("s1", "s2"))

  srcpkg <- srcpkg(pkg)

  .comma <- function(x) paste0(x, collapse = ",")
  expect_identical(srcpkg$imports, .comma(c("i1", "i2")))
  expect_identical(srcpkg$depends, .comma("d1"))
  expect_identical(srcpkg$suggests, .comma(c("s1", "s2")))
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






