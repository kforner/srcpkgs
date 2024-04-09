
test_that("fetch_srcpkgs_meta", {
  setup_temp_dir()
  pkg1 <- pkg_create('.', 'pkg1')
  pkg2 <- pkg_create('.', 'pkg2')

  ### no dev meta --> no srcpkg meta data
  # pkg not loaded (by devtools at least) 
  expect_null(fetch_srcpkg_meta('pkg1'))
  # pkg loaded (base) but not by devtools
  expect_null(fetch_srcpkg_meta('devtools'))

  expect_null(fetch_srcpkgs_meta())

  ## load pkg1
  on.exit(unloadNamespace('pkg1'), add = TRUE)
  devtools::load_all('pkg1', quiet = TRUE, attach = FALSE) 

  ## still no srcpkg meta
  expect_null(fetch_srcpkg_meta('pkg1'))
  expect_null(fetch_srcpkgs_meta())

  ## store the srcpkg in meta data
  srcpkg1 <- srcpkg(pkg1)
  store_srcpkg_meta(srcpkg1)

  # now that works
  expect_identical(fetch_srcpkg_meta('pkg1'), srcpkg1)

  # fetch_srcpkgs_meta() also works
  df <- fetch_srcpkgs_meta()

  expect_true(is.data.frame(df))
  expect_identical(df$package, 'pkg1')
  expect_false(df$attached)


  ### load pkg2, and attach it
  on.exit(unloadNamespace('pkg2'), add = TRUE)
  devtools::load_all('pkg2', quiet = TRUE, attach = TRUE) 

  # no meta data yet
  expect_identical(fetch_srcpkgs_meta()$package, 'pkg1')

  # store its meta data
  srcpkg2 <- srcpkg(pkg2)
  store_srcpkg_meta(srcpkg2)

  # check
  expect_identical(fetch_srcpkg_meta('pkg2'), srcpkg2)

  # fetch_srcpkgs_meta() also works
  df <- fetch_srcpkgs_meta()
  
  expect_true(is.data.frame(df))
  expect_setequal(df$package, c('pkg1', 'pkg2'))
  expect_setequal(df$attached, c(TRUE, FALSE))

  # unload them
  unloadNamespace('pkg1')
  unloadNamespace('pkg2')

  # no longer works
  expect_null(fetch_srcpkg_meta('pkg1'))
})
