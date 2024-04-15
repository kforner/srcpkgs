test_that("pkg_just_load_pkg", {
  setup_temp_dir()
  pkg_create('.', 'AA')
  on.exit(pkg_unload('AA', quiet = TRUE), add = TRUE)

  code <- "
#' exported stuff
#' @return stuff
#' @export
stuff <- function() { 'stuffed'}
  "
  writeLines(code, 'AA/R/stuff.R')

  PKG <- srcpkg(path = 'AA')

  pkg_roxygenise(PKG$path, quiet = TRUE)
  pkg_unload('AA', quiet = TRUE)

  ### normal package - attach - export_all=FALSE
  expect_true(pkg_just_load_pkg(PKG, quiet = TRUE))

  expect_true(pkg_is_attached(PKG))
  # the stuff function should be available
  expect_identical(stuff(), 'stuffed')
  # but not the dummy function since export_all=FALSE
  expect_false(exists('dummy'))
  pkg_unload('AA', quiet = TRUE)

  ### export_all
  expect_true(pkg_just_load_pkg(PKG, quiet = TRUE, export_all = TRUE))

  expect_identical(stuff(), 'stuffed')
  expect_identical(dummy(), 'DUMMY')
  pkg_unload('AA', quiet = TRUE)

  ### attach = FALSE
  expect_true(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE))

  expect_true(pkg_is_loaded(PKG))
  expect_false(pkg_is_attached(PKG))
  pkg_unload('AA', quiet = TRUE)

  ### try to load twice the same package
  expect_true(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE))
  # N.B: the 2nd time, it is not reloaded
  expect_false(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE))
  pkg_unload('AA', quiet = TRUE)

  ### error during load
  cat('stop("Argh")', file = 'AA/R/error.R')

  expect_error(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE), 'Argh')

  expect_false(pkg_is_loaded(PKG))
})



test_that("pkg_needs_reload", {
  setup_temp_dir()
  pkg_create('.', 'AA')
  on.exit(pkg_unload('AA', quiet = TRUE), add = TRUE)
  pkg_unload('AA', quiet = TRUE)
  PKG <- srcpkg(path = 'AA')
  
  ### not loaded yet
  expect_true(pkg_needs_reload('AA'))

  ### loaded via srcpkg: 
  pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE)
  expect_false(pkg_needs_reload('AA'))

  # unloaded
  pkg_unload(PKG, quiet = TRUE)
  expect_true(pkg_needs_reload('AA'))

  # loaded, but no roxygen
  pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE)
  pkg_delete_doc(PKG$path)
  expect_true(pkg_needs_reload('AA', quiet = TRUE))

  # unloaded
  pkg_unload(PKG, quiet = TRUE)
  expect_true(pkg_needs_reload('AA'))

  # loaded via load_all --> needs reload
  devtools::load_all(PKG, quiet = TRUE)
  expect_true(pkg_needs_reload('AA'))
  pkg_unload(PKG, quiet = TRUE)

  # loaded via library
  expect_false(pkg_needs_reload('base'))
})
