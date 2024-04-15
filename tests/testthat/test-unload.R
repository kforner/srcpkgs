
test_that("pkg_unload", {
  setup_temp_dir()

  pkg_create('.', 'AA')
  pkg_create('.', 'nsimports', imports = 'AA', namespace = TRUE)
  pkg_create('.', 'nsimports2', imports = 'nsimports', namespace = TRUE)
  pkg_create('.', 'descimports', imports = 'AA', namespace = FALSE)
  pkg_create('.', 'descdepends', depends = 'AA')

  on.exit({
    unloadNamespace('descdepends')
    unloadNamespace('descimports')
    unloadNamespace('nsimports2')
    unloadNamespace('nsimports')
    unloadNamespace('AA')
  }, add = TRUE)


  pkg_unload('AA', quiet = TRUE) # just in case...
  #### not loaded
  expect_null(pkg_unload('AA')) 

  ### no dependencies (yet)
  devtools::load_all('AA', export_all = FALSE, attach = TRUE, quiet = TRUE)

  df <- pkg_unload('AA', quiet = TRUE)

  expect_false(pkg_is_loaded('AA'))
  expect_true(is.data.frame(df))
  expect_identical(df$pkg, 'AA')
  expect_true(df$attached)

  ### with all dependencies
  devtools::load_all('AA', export_all = FALSE, attach = TRUE, quiet = TRUE)
  devtools::load_all('nsimports', export_all = FALSE, attach = FALSE, quiet = TRUE)
  devtools::load_all('nsimports2', export_all = FALSE, attach = FALSE, quiet = TRUE)
  devtools::load_all('descimports', export_all = FALSE, attach = FALSE, quiet = TRUE)
  devtools::load_all('descdepends', export_all = FALSE, attach = FALSE, quiet = TRUE)

  df <- pkg_unload('AA', quiet = TRUE)

  expect_false(pkg_is_loaded('AA'))
  expect_false(pkg_is_loaded('nsimports'))
  expect_false(pkg_is_loaded('nsimports2'))
  expect_true(pkg_is_loaded('descimports'))
  expect_true(pkg_is_loaded('descdepends'))

  expect_setequal(df$pkg, c('nsimports2', 'nsimports', 'AA'))
})


test_that("find_loaded_packages_namespace_imports", {
  setup_temp_dir()

  pkg_create('.', 'AA', imports = c('tools', 'devtools'), namespace = TRUE)
  pkg_create('.', 'BB', imports = 'AA', namespace = TRUE)
  pkg_create('.', 'CC', imports = 'BB', depends = 'AA', namespace = TRUE)
  pkg_create('.', 'descimports', imports = c('testthat', 'utils'), namespace = FALSE)

  # our test packages are not loaded yet
  imports <- find_loaded_packages_namespace_imports()

  expect_true(is.list(imports))
  # not yet loaded
  expect_false(any(c('AA', 'BB', 'CC', 'descimports') %in% names(imports)))
  # testthat should be there
  expect_true('rlang' %in% imports[['testthat']])

  # now load the packages
  on.exit({
    unloadNamespace('CC')
    unloadNamespace('BB')
    unloadNamespace('AA')
    unloadNamespace('descimports')
  }, add = TRUE)

  # N.B: attaching AA because it is a "depends" of CC
  devtools::load_all('AA', export_all = FALSE, attach = TRUE, quiet = TRUE)
  devtools::load_all('BB', export_all = FALSE, attach = FALSE, quiet = TRUE)
  devtools::load_all('CC', export_all = FALSE, attach = FALSE, quiet = TRUE)
  devtools::load_all('descimports', export_all = FALSE, attach = FALSE, quiet = TRUE)

  imports <- find_loaded_packages_namespace_imports()

  expect_true(all(c('AA', 'BB', 'CC') %in% names(imports)))

  expect_null(imports[['descimports']])
  expect_identical(imports[['AA']],  c('devtools', 'tools')) # N.B: sorted
  expect_identical(imports[['BB']],  'AA')
  expect_identical(imports[['CC']],  'BB') # N.B; 'AA' is not listed since it is a "Depends"
})

test_that("load/unload POC - NAMESPACE imports", {
  setup_temp_dir()

  pkg_create('.', 'AA')
  pkg_create('.', 'BB', imports = 'AA', namespace = TRUE)

  on.exit({
    unloadNamespace('BB')
    unloadNamespace('AA')
  }, add = TRUE)

  # BB can not be loaded because AA is a NS-import and is not loaded
  expect_error(devtools::load_all('BB', export_all = FALSE, quiet = TRUE), 'required')
  
  # load AA...
  devtools::load_all('AA', export_all = FALSE,  quiet = TRUE)

  # and now BB CAN be loaded
  devtools::load_all('BB', export_all = FALSE, quiet = TRUE)

  ### unload
  # for the same reason, AA can NOT be unloaded because it is NS-imported by BB
  expect_error(unloadNamespace('AA'), 'cannot be unloaded')

  # but if we first unload BB...
  unloadNamespace('BB')

  # then we CAN unload AA
  expect_error(unloadNamespace('AA'), NA)
})


# cf "load/unload POC - NAMESPACE imports" above
test_that("load/unload POC - DESCRIPTION imports", {
  setup_temp_dir()

  pkg_create('.', 'AA')
  pkg_create('.', 'BB', imports = 'AA', namespace = FALSE)

  on.exit({
    unloadNamespace('BB')
    unloadNamespace('AA')
  }, add = TRUE)

  ### load: same behaviour as the NS-imports
  #  BB can not be loaded because AA is an import and is not loaded nor installed
  expect_error(devtools::load_all('BB', export_all = FALSE, quiet = TRUE), 'required')

  # load AA...
  devtools::load_all('AA', export_all = FALSE,  quiet = TRUE)

  # and now BB CAN be loaded
  devtools::load_all('BB', export_all = FALSE, quiet = TRUE)

  ### unload: different behaviour !
  # AA CAN be unloaded because it is only DESC-imported by BB
  expect_error(unloadNamespace('AA'), NA)

  # then we CAN unload BB
  expect_error(unloadNamespace('BB'), NA)
})


test_that("load/unload POC - DESCRIPTION depends", {
  setup_temp_dir()

  pkg_create('.', 'AA')
  pkg_create('.', 'BB', depends = 'AA')

  on.exit({
    unloadNamespace('BB')
    unloadNamespace('AA')
  }, add = TRUE)

  ### load: same behaviour as the NS-imports
  #  BB can not be loaded because AA is an import and is not loaded nor installed
  expect_error(devtools::load_all('BB', export_all = FALSE, quiet = TRUE), 'required')

  # load AA...
  devtools::load_all('AA', export_all = FALSE, attach = FALSE,  quiet = TRUE)
  expect_false(pkg_is_attached('AA'))

  # BB can be loaded BUT with a WARNING since AA is not attached
  expect_warning(devtools::load_all('BB', export_all = FALSE, quiet = TRUE), 'AA')

  # attach AA
  unloadNamespace('BB')
  attachNamespace('AA')
  expect_true(pkg_is_attached('AA'))
  
  # NOW BB CAN be loaded because AA is attached
  expect_error(devtools::load_all('BB', export_all = FALSE, quiet = TRUE), NA)

  ### unload: 
  expect_error(pkg_detach('AA'), 'required')

  expect_error(unloadNamespace('BB'), NA)

  # now we can detach AA
  expect_error(pkg_detach('AA'), NA)

  # or unload it
  expect_error(unloadNamespace('AA'), NA)
})
