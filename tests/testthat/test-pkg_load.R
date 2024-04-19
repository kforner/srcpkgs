test_that("pkg_load - complex example ", {
  setup_temp_dir()
  # C-->B-->A, F-->D-->B, E-->A, Z
  # N.B: we use namespace = TRUE because for unloading, only the ns-imports are considered
  pkg_create('.', 'AA', imports = 'stats', namespace = TRUE)
  pkg_create('.', 'BB', imports = 'AA', namespace = TRUE)
  pkg_create('.', 'CC', imports = 'BB', namespace = TRUE)
  pkg_create('.', 'DD', imports = 'BB', namespace = TRUE)
  pkg_create('.', 'FF', imports = 'DD', namespace = TRUE)
  pkg_create('.', 'EE', imports = 'AA', namespace = TRUE)
  pkg_create('.', 'ZZ')

  .cleanup <- function() {
    for (pkg in c('AA', 'BB', 'CC', 'DD', 'EE', 'FF', 'ZZ')) pkg_unload(pkg, quiet = TRUE)
  }
  on.exit(.cleanup(), add = TRUE)
  .cleanup()

  src_pkgs <- find_srcpkgs('.')
  deps_graph <- compute_pkgs_dependencies_graph(src_pkgs)
  .load <- function(pkg) pkg_load(pkg, src_pkgs, deps_graph, roxygen = FALSE, quiet = TRUE)
  .unload <- function(pkg) pkg_unload(pkg, quiet = TRUE)
  .modify <- function(pkg_name) cat('\n', file = file.path(pkg_name, 'DESCRIPTION'), append = TRUE)
  #############################################################################################
  ### load F
  expect_true(.load('FF'))
  # reload 
  expect_false(.load('FF'))

  ### unload B
  .unload('BB')
  expect_false(pkg_is_loaded('BB'))
  # F was unloaded since F -->...-->B
  expect_false(pkg_is_loaded('FF'))

  ### reload F
  expect_true(.load('FF'))

  ### unload A --> unload all dependents

  df <- .unload('AA')
  expect_setequal(df$pkg, c('FF', 'DD', 'BB', 'AA'))

  expect_true(.load('FF'))

  ### load other not-yet-loaded packages
  expect_true(.load('CC'))
  expect_true(.load('EE'))
  expect_true(.load('ZZ'))

  ####################### modify packages: simulate edition ################
  ### Z --> should NOT invalidate F
  .modify('ZZ')
  expect_false(.load('FF'))
  ### E: should NOT invalidate F
  .modify('EE')
  expect_false(.load('FF'))
  ## B: SHOULD invalidate F
  .modify('BB')
  expect_true(.load('FF')) 
  # C was unloaded because B has to be reloaded, and should have been restored
  expect_true(pkg_is_loaded('CC'))
})




test_that("pkg_load_dependencies - transitivity ", {
  # the deps graph is A-->B--C
  setup_temp_dir()

  pkg_create('.', 'pkgA', imports  = 'pkgB')
  pkg_create('.', 'pkgB', imports  = 'pkgC')
  pkg_create('.','pkgC', suggests = 'roxygen2')

  .cleanup <- function() for (pkg in c('pkgA', 'pkgB', 'pkgC')) pkg_unload(pkg, quiet = TRUE)
  on.exit(.cleanup(), add = TRUE)

  src_pkgs <- find_srcpkgs('.')
  deps_graph <- compute_pkgs_dependencies_graph(src_pkgs)

  # N.B: we use here roxygen = FALSE to speed up the tests
  expect_true( pkg_load_dependencies('pkgA', src_pkgs, deps_graph, 
    ignore_suggests = TRUE, quiet = TRUE, roxygen = FALSE))
  expect_true(pkg_is_loaded('pkgA'))
})

test_that("pkg_load_dependencies - star example ", {
  # the deps graph is a star centered at pkgE
  setup_temp_dir()

  pkg_create('.', 'pkgA', suggests = 'roxygen2')
  pkg_create('.', 'pkgB', suggests = 'roxygen2')
  pkg_create('.','pkgC', suggests = 'roxygen2')
  pkg_create('.','pkgD')
  pkg_create('.','pkgE',
    imports = c('pkgA', 'pkgB'),
    depends = c('pkgB', 'pkgD'),
    suggests = c('pkgC', 'roxygen2'))

  .cleanup <- function() {
    for (pkg in c('pkgA', 'pkgB', 'pkgC', 'pkgD', 'pkgE')) pkg_unload(pkg, quiet = TRUE)
  }
  on.exit(.cleanup(), add = TRUE)

  src_pkgs <- find_srcpkgs('.')
  deps_graph <- compute_pkgs_dependencies_graph(src_pkgs)
  .load <- function(pkg, ignore_suggests = FALSE, ...) {
    pkg_load_dependencies(pkg, src_pkgs, deps_graph, ignore_suggests = ignore_suggests, 
      quiet = TRUE, roxygen = FALSE, ...)
  }
  .modify <- function(pkg_name) cat('\n', file = file.path(pkg_name, 'DESCRIPTION'), append = TRUE)

  ###############################################################################################

  ### load pkgE
  expect_true(.load('pkgE'))
  # imports were loaded but not attached
  expect_true(pkg_is_loaded('pkgA') && !pkg_is_attached('pkgA'))
  # depends were loaded AND attached
  expect_true(pkg_is_loaded('pkgB') && pkg_is_attached('pkgB'))
  expect_true(pkg_is_loaded('pkgD') && pkg_is_attached('pkgD'))
  # suggests were lot loaded (as they should)
  expect_false(pkg_is_loaded('pkgC'))

  ### load again pkgE
  # N.B: FALSE since no package needed to be (re)loaded
  expect_false(.load('pkgE'))

  ### now unload one pkgE import then reload pkgE
  pkg_unload('pkgA', quiet = TRUE)
  expect_false(pkg_is_loaded('pkgA'))

  # now pkgA needed to be reloaded
  expect_true(.load('pkgE'))
  # and pkgA was indeed loaded
  expect_true(pkg_is_loaded('pkgA') && !pkg_is_attached('pkgA'))

  ### unload one pkgE depends then reload pkgE
  pkg_unload('pkgD', quiet = TRUE)
  expect_false(pkg_is_loaded('pkgD'))
  
  expect_true(.load('pkgE'))
  # and pkgD was indeed attached
  expect_true(pkg_is_loaded('pkgD') && pkg_is_attached('pkgB'))

  ### modify an import
  .modify('pkgA')
  expect_true(.load('pkgE'))
  expect_true(pkg_is_loaded('pkgA') && !pkg_is_attached('pkgA'))

  ### modify a depend
  .modify('pkgD')
  expect_true(.load('pkgE'))
  expect_true(pkg_is_attached('pkgD'))

  # modify pkgE
  .modify('pkgE')
  expect_true(.load('pkgE'))
  expect_true(pkg_is_attached('pkgE'))

  # modify a unloaded suggest
  .modify('pkgC')
  expect_false(.load('pkgE'))
  expect_false(pkg_is_loaded('pkgC'))

  # modify a loaded suggest
  .load('pkgC')
  .modify('pkgC')
  expect_true(.load('pkgE'))
  expect_true(pkg_is_loaded('pkgC'))
})


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
