test_that("non_srcpkg_unload_plan", {
  plan <- non_srcpkg_unload_plan('rlang', loaded = c('devtools', 'pkgload', 'rlang', 'stats'))
  expect_identical(plan$package, c('devtools', 'pkgload', 'rlang'))
  expect_identical(unique(plan$action), 'unload')
})


test_that("srcpkg_unload_plan", {
  setup_temp_dir()
  src_pkgs <- examples_srcpkgs_complex_deps()
  on.exit(cleanup_dangling_srcpkgs(), add = TRUE)

  ### nothing to unload
  expect_null(srcpkg_unload_plan('EE', src_pkgs, loaded = NULL))
  
  ### simulate all loaded 
  # unload E --> need to unload all but Z
  plan <- srcpkg_unload_plan('EE', src_pkgs, loaded = names(src_pkgs))
  expect_identical(plan$package, c('AA', 'BB', 'CC', 'DD', 'EE'))
  expect_identical(unique(plan$action), 'unload')

  # unload Z -> only Z
  plan <- srcpkg_unload_plan('ZZ', src_pkgs, loaded = names(src_pkgs))
  expect_identical(plan$package, 'ZZ')
  # same for A
  plan <- srcpkg_unload_plan('AA', src_pkgs, loaded = names(src_pkgs))
  expect_identical(plan$package, 'AA')
  # for D
  plan <- srcpkg_unload_plan('DD', src_pkgs, loaded = names(src_pkgs))
  expect_identical(plan$package, c('AA', 'BB', 'CC', 'DD'))

  ### not all loaded
  expect_null(srcpkg_unload_plan('EE', src_pkgs, loaded = 'DD'))
  
  plan <- srcpkg_unload_plan('EE', src_pkgs, loaded = c('DD', 'EE'))
  expect_identical(plan$package, c('DD', 'EE'))

  # this should not be possible, since A depends on B. nonetheless we should then unload also A
  plan <- srcpkg_unload_plan('EE', src_pkgs, loaded = c('AA', 'DD', 'EE'))
  expect_identical(plan$package, c('AA', 'DD', 'EE'))
})


test_that("unload_plan", {
  ### A->B-C, A->C
  mat <- graph_from_strings('A->B->C', 'B->D')
  NONE <- character()
  ALL <- c('A', 'B', 'C', 'D')
  # N.B: A,B,D,C would have also worked

  expect_identical(unload_plan(c('C', 'D'), mat, loaded = ALL)$package, c('A', 'B', 'C', 'D'))

  expect_identical(unload_plan('C', mat, loaded = ALL)$package, c('A', 'B', 'C'))
  expect_identical(unload_plan('D', mat, loaded = ALL)$package, c('A', 'B', 'D'))
  expect_identical(unload_plan('B', mat, loaded = ALL)$package, c('A', 'B'))

  # using loaded=
  expect_null(unload_plan(c('C', 'D'), mat, loaded = NONE))
  expect_null(unload_plan('D', mat, loaded = NONE))
  expect_identical(unload_plan(c('C', 'D'), mat, loaded = c('C', 'D'))$package, c('C', 'D'))
  expect_identical(unload_plan(c('C', 'D'), mat, loaded = c('C', 'D'))$package, c('C', 'D'))

  ### empty matrix
  mat <- matrix(0L, 0, 0)
  expect_null(unload_plan('A', mat, loaded = ALL))

  ### trivial: A
  mat <- graph_from_strings('A->A')
  mat[1,1] <- 0L
  
  plan <- unload_plan('A', mat, loaded = ALL)
  expect_identical(plan, data.frame(package = 'A', action = 'unload'))

  ### simple : A-> B
  mat <- graph_from_strings('A->B')

  expect_identical(unload_plan('A', mat, loaded = ALL), data.frame(package = 'A', action = 'unload'))
  expect_identical(unload_plan('B', mat, loaded = ALL), data.frame(package = c('A','B'), action = 'unload'))

  ###  A-> B -> C
  mat <- graph_from_strings('A->B->C')

  expect_identical(unload_plan('A', mat, loaded = ALL), data.frame(package = 'A', action = 'unload'))
  expect_identical(unload_plan('B', mat, loaded = ALL), data.frame(package = c('A','B'), action = 'unload'))
  expect_identical(unload_plan('C', mat, loaded = ALL), 
    data.frame(package = c('A','B', 'C'), action = rep('unload', 3)))
  
  # 
  mat <- graph_from_strings('A->B->C', 'B->D->C')
  
  expect_identical(unload_plan('A', mat, loaded = ALL), data.frame(package = 'A', action = 'unload'))
  expect_identical(unload_plan('D', mat, loaded = ALL), 
    data.frame(package = c('A','B', 'D'), action = 'unload'))
  expect_identical(unload_plan('C', mat, loaded = ALL), 
    data.frame(package = c('A','B', 'D', 'C'), action = 'unload'))
})


test_that("pkg_unload", {
  setup_temp_dir()
  all_src_pkgs <- examples_srcpkgs_complex_deps()
  on.exit(cleanup_dangling_srcpkgs(), add =TRUE)

  ### srcpkg

  .unload <- function(x, dry_run = TRUE, quiet = TRUE, src_pkgs = all_src_pkgs, ...) {
    force(src_pkgs)
    my_src_pkgs <- src_pkgs
    pkg_unload(x, src_pkgs = my_src_pkgs, dry_run = dry_run, quiet = quiet, ...)
  }

  # not loaded
  expect_null(.unload('AA', loaded = NULL))

  # all loaded
  plan <- .unload('EE', loaded = names(all_src_pkgs))
  expect_identical(plan$package, c('AA', 'BB', 'CC', 'DD', 'EE'))
  
  plan <- .unload('BB', loaded = names(all_src_pkgs))
  expect_identical(plan$package, c('AA', 'BB'))

  # actually load/unload something
  for(pkg in all_src_pkgs) .unload(pkg, dry_run = FALSE)

  devtools::load_all('EE', export_all = FALSE, attach = TRUE, quiet = TRUE)
  devtools::load_all('DD', export_all = FALSE, attach = FALSE, quiet = TRUE)
 
  plan <- .unload('EE', dry_run = FALSE)

  expect_identical(plan$package, c('DD', 'EE'))
  expect_false(pkg_is_loaded('DD'))
  expect_false(pkg_is_loaded('EE'))
  ### non srcpkg

  # not loaded
  expect_null(.unload('rlang', loaded = NULL))

  # all loaded
  plan <- .unload('rlang', loaded = c('devtools', 'pkgload', 'rlang'))
  expect_identical(plan$package, c('devtools', 'pkgload', 'rlang'))
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
