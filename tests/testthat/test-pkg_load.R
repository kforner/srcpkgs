test_that("load_plan", {
  ### A->C->D, B->D
  mat <- graph_from_strings('A->C->D', 'B->D')
  NONE <- character()
  # N.B: A,B,D,C would have also worked
  plan <- load_plan(c('A', 'B'), mat, loaded = NONE)
  expect_identical(unique(plan$action), 'load')
  expect_identical(plan$package, c('D', 'B', 'C', 'A'))

  expect_identical(load_plan('A', mat)$package, c('D', 'C', 'A'))
  expect_identical(load_plan('B', mat)$package, c('D', 'B'))
  expect_identical(load_plan('C', mat)$package, c('D', 'C'))
  expect_identical(load_plan('D', mat)$package, 'D')
})

test_that("pkg_load_full_plan() - transitivity ", {
  # the deps graph is A-->B--C
  setup_temp_dir()

  pkg_create('.', 'pkgA', imports  = 'pkgB')
  pkg_create('.', 'pkgB', imports  = 'pkgC')
  pkg_create('.','pkgC', suggests = 'roxygen2')
  cleanup_dangling_srcpkgs()
  on.exit(cleanup_dangling_srcpkgs, add = TRUE)
  src_pkgs <- find_srcpkgs('.')
  NONE <- character()
  ALL <- names(src_pkgs)

  ################### load A #################################################

  ### all loaded and outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = ALL)
  expected <- data.frame(
    package = c("pkgA", "pkgB", "pkgC", "pkgC", "pkgB", "pkgA"),
    action = c("unload", "unload", "unload", "doc_and_load", "doc_and_load", "doc_and_load"))
  expect_identical(plan, expected)
  
  ### all loaded, none outdated
  expect_null(pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = NONE))
browser()
  ### all loaded, A outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = 'pkgA')
  expected <- data.frame(package = c("pkgA", "pkgA"), action = c("unload", "doc_and_load"))
  expect_identical(plan, expected)
  
  ### all loaded, C outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = 'pkgC')
  expected <- data.frame(
    package = c("pkgA", "pkgB", "pkgC", "pkgC", "pkgB", "pkgA"),
    action = c("unload", "unload", "unload", "doc_and_load", "load", "load"))
  expect_identical(plan, expected)
  
  ### none loaded, none outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = NONE, outdated = NONE)
  expected <- data.frame(
    package = c("pkgC", "pkgB", "pkgA"),
    action = c("load", "load", "load"))
  expect_identical(plan, expected)
  
  ### none loaded, all outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = NONE, outdated = ALL)
  expected <- data.frame(
    package = c("pkgC", "pkgB", "pkgA"),
    action = c("doc_and_load", "doc_and_load", "doc_and_load"))
  expect_identical(plan, expected)

  ### none loaded, C outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = NONE, outdated = 'pkgC')
  expected <- data.frame(
    package = c("pkgC", "pkgB", "pkgA"),
    action = c("doc_and_load", "load", "load"))
  expect_identical(plan, expected)

  ### A loaded, B outdated (N.B: that should not be possible)
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = 'pkgA', outdated = 'pkgB')
  expected <- data.frame(
    package = c("pkgC", "pkgB"),
    action = c("load", "doc_and_load"))
  expect_identical(plan, expected)

  ################### load C #################################################
  ### all loaded and outdated
  plan <- pkg_load_full_plan('pkgC', src_pkgs, loaded = ALL, outdated = ALL)
  expected <- data.frame(
    package = c("pkgA", "pkgB", "pkgC", "pkgC", "pkgB", "pkgA"),
    action = c("unload", "unload", "unload", "doc_and_load", "doc_and_load", "doc_and_load"))
  expect_identical(plan, expected)

  browser()
})

# test_that("pkg_load - complex example ", {
#   setup_temp_dir()

#   src_pkgs <- examples_srcpkgs_complex_imports()
#   on.exit(cleanup_dangling_srcpkgs(), add = TRUE)


#   deps_graph <- compute_pkgs_dependencies_graph(src_pkgs)
#   .load <- function(pkg) pkg_load(pkg, src_pkgs, deps_graph, roxygen = FALSE, quiet = TRUE)
#   .unload <- function(pkg) pkg_unload(pkg, quiet = TRUE)
#   .modify <- function(pkg_name) cat('\n', file = file.path(pkg_name, 'DESCRIPTION'), append = TRUE)
#   #############################################################################################
#   ### load F
#   expect_true(.load('FF'))
#   # reload 
#   expect_false(.load('FF'))

#   ### unload B
#   .unload('BB')
#   expect_false(pkg_is_loaded('BB'))
#   # F was unloaded since F -->...-->B
#   expect_false(pkg_is_loaded('FF'))

#   ### reload F
#   expect_true(.load('FF'))

#   ### unload A --> unload all dependents

#   df <- .unload('AA')
#   expect_setequal(df$pkg, c('FF', 'DD', 'BB', 'AA'))

#   expect_true(.load('FF'))

#   ### load other not-yet-loaded packages
#   expect_true(.load('CC'))
#   expect_true(.load('EE'))
#   expect_true(.load('ZZ'))

#   ####################### modify packages: simulate edition ################
#   ### Z --> should NOT invalidate F
#   .modify('ZZ')
#   expect_false(.load('FF'))
#   ### E: should NOT invalidate F
#   .modify('EE')
#   expect_false(.load('FF'))
#   ## B: SHOULD invalidate F
#   .modify('BB')
#   expect_true(.load('FF')) 
#   # C was unloaded because B has to be reloaded, and should have been restored
#   expect_true(pkg_is_loaded('CC'))
# })




# test_that("pkg_load_dependencies - transitivity ", {
#   # the deps graph is A-->B--C
#   setup_temp_dir()

#   pkg_create('.', 'pkgA', imports  = 'pkgB')
#   pkg_create('.', 'pkgB', imports  = 'pkgC')
#   pkg_create('.','pkgC', suggests = 'roxygen2')

#   .cleanup <- function() for (pkg in c('pkgA', 'pkgB', 'pkgC')) pkg_unload(pkg, quiet = TRUE)
#   on.exit(.cleanup(), add = TRUE)

#   src_pkgs <- find_srcpkgs('.')
#   deps_graph <- compute_pkgs_dependencies_graph(src_pkgs)

#   # N.B: we use here roxygen = FALSE to speed up the tests
#   expect_true( pkg_load_dependencies('pkgA', src_pkgs, deps_graph, 
#     ignore_suggests = TRUE, quiet = TRUE, roxygen = FALSE))
#   expect_true(pkg_is_loaded('pkgA'))
# })

# test_that("pkg_load_dependencies - star example ", {
#   # the deps graph is a star centered at pkgE
#   setup_temp_dir()

#   pkg_create('.', 'pkgA', suggests = 'roxygen2')
#   pkg_create('.', 'pkgB', suggests = 'roxygen2')
#   pkg_create('.','pkgC', suggests = 'roxygen2')
#   pkg_create('.','pkgD')
#   pkg_create('.','pkgE',
#     imports = c('pkgA', 'pkgB'),
#     depends = c('pkgB', 'pkgD'),
#     suggests = c('pkgC', 'roxygen2'))

#   .cleanup <- function() {
#     for (pkg in c('pkgA', 'pkgB', 'pkgC', 'pkgD', 'pkgE')) pkg_unload(pkg, quiet = TRUE)
#   }
#   on.exit(.cleanup(), add = TRUE)

#   src_pkgs <- find_srcpkgs('.')
#   deps_graph <- compute_pkgs_dependencies_graph(src_pkgs)
#   .load <- function(pkg, ignore_suggests = FALSE, ...) {
#     pkg_load_dependencies(pkg, src_pkgs, deps_graph, ignore_suggests = ignore_suggests, 
#       quiet = TRUE, roxygen = FALSE, ...)
#   }
#   .modify <- function(pkg_name) cat('\n', file = file.path(pkg_name, 'DESCRIPTION'), append = TRUE)

#   ###############################################################################################

#   ### load pkgE
#   expect_true(.load('pkgE'))
#   # imports were loaded but not attached
#   expect_true(pkg_is_loaded('pkgA') && !pkg_is_attached('pkgA'))
#   # depends were loaded AND attached
#   expect_true(pkg_is_loaded('pkgB') && pkg_is_attached('pkgB'))
#   expect_true(pkg_is_loaded('pkgD') && pkg_is_attached('pkgD'))
#   # suggests were lot loaded (as they should)
#   expect_false(pkg_is_loaded('pkgC'))

#   ### load again pkgE
#   # N.B: FALSE since no package needed to be (re)loaded
#   expect_false(.load('pkgE'))

#   ### now unload one pkgE import then reload pkgE
#   pkg_unload('pkgA', quiet = TRUE)
#   expect_false(pkg_is_loaded('pkgA'))

#   # now pkgA needed to be reloaded
#   expect_true(.load('pkgE'))
#   # and pkgA was indeed loaded
#   expect_true(pkg_is_loaded('pkgA') && !pkg_is_attached('pkgA'))

#   ### unload one pkgE depends then reload pkgE
#   pkg_unload('pkgD', quiet = TRUE)
#   expect_false(pkg_is_loaded('pkgD'))
  
#   expect_true(.load('pkgE'))
#   # and pkgD was indeed attached
#   expect_true(pkg_is_loaded('pkgD') && pkg_is_attached('pkgB'))

#   ### modify an import
#   .modify('pkgA')
#   expect_true(.load('pkgE'))
#   expect_true(pkg_is_loaded('pkgA') && !pkg_is_attached('pkgA'))

#   ### modify a depend
#   .modify('pkgD')
#   expect_true(.load('pkgE'))
#   expect_true(pkg_is_attached('pkgD'))

#   # modify pkgE
#   .modify('pkgE')
#   expect_true(.load('pkgE'))
#   expect_true(pkg_is_attached('pkgE'))

#   # modify a unloaded suggest
#   .modify('pkgC')
#   expect_false(.load('pkgE'))
#   expect_false(pkg_is_loaded('pkgC'))

#   # modify a loaded suggest
#   .load('pkgC')
#   .modify('pkgC')
#   expect_true(.load('pkgE'))
#   expect_true(pkg_is_loaded('pkgC'))
# })


# test_that("pkg_just_load_pkg", {
#   setup_temp_dir()
#   pkg_create('.', 'AA')
#   on.exit(pkg_unload('AA', quiet = TRUE), add = TRUE)

#   code <- "
# #' exported stuff
# #' @return stuff
# #' @export
# stuff <- function() { 'stuffed'}
#   "
#   writeLines(code, 'AA/R/stuff.R')

#   PKG <- srcpkg(path = 'AA')

#   pkg_roxygenise(PKG$path, quiet = TRUE)
#   pkg_unload('AA', quiet = TRUE)

#   ### normal package - attach - export_all=FALSE
#   expect_true(pkg_just_load_pkg(PKG, quiet = TRUE))

#   expect_true(pkg_is_attached(PKG))
#   # the stuff function should be available
#   expect_identical(stuff(), 'stuffed')
#   # but not the dummy function since export_all=FALSE
#   expect_false(exists('dummy'))
#   pkg_unload('AA', quiet = TRUE)

#   ### export_all
#   expect_true(pkg_just_load_pkg(PKG, quiet = TRUE, export_all = TRUE))

#   expect_identical(stuff(), 'stuffed')
#   expect_identical(dummy(), 'DUMMY')
#   pkg_unload('AA', quiet = TRUE)

#   ### attach = FALSE
#   expect_true(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE))

#   expect_true(pkg_is_loaded(PKG))
#   expect_false(pkg_is_attached(PKG))
#   pkg_unload('AA', quiet = TRUE)

#   ### try to load twice the same package
#   expect_true(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE))
#   # N.B: the 2nd time, it is not reloaded
#   expect_false(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE))
#   pkg_unload('AA', quiet = TRUE)

#   ### error during load
#   cat('stop("Argh")', file = 'AA/R/error.R')

#   expect_error(pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE), 'Argh')

#   expect_false(pkg_is_loaded(PKG))
# })


test_that("pkg_is_outdated", {
  setup_temp_dir()
  pkg_create('.', 'AA')
  cleanup_dangling_srcpkgs()
  on.exit(cleanup_dangling_srcpkgs, add = TRUE)
  PKG <- srcpkg(path = 'AA')

  ### no doc
  expect_true(pkg_is_outdated('AA', quiet = FALSE))
  
  ### loaded via srcpkg: --> up-to-date
  pkg_load_wrapper(PKG, quiet = TRUE, attach = FALSE)
  expect_false(pkg_is_outdated('AA', quiet = FALSE))

  # unloaded --> still up-to-date
  pkg_unload(PKG, quiet = TRUE)
  expect_false(pkg_is_outdated('AA', quiet = FALSE))

  # no roxygen
  pkg_delete_doc(PKG$path)
  expect_true(pkg_is_outdated('AA', quiet = FALSE))

  # roxygenise..
  pkg_roxygenise_wrapper(PKG$path, quiet = TRUE)
  expect_false(pkg_is_outdated('AA', quiet = TRUE))

  # modify a source
  writeLines('# hello', 'AA/R/toto.R')
  expect_true(pkg_is_outdated('AA', quiet = TRUE))

  # loaded via load_all --> needs reload
  pkg_unload(PKG, quiet = TRUE)
  pkg_delete_md5sum(PKG$path)
  devtools::load_all(PKG, quiet = TRUE, attach = FALSE)

  expect_true(pkg_is_outdated(PKG$path, quiet = FALSE))
})



# test_that("pkg_needs_reload", {
#   setup_temp_dir()
#   pkg_create('.', 'AA')
#   on.exit(pkg_unload('AA', quiet = TRUE), add = TRUE)
#   pkg_unload('AA', quiet = TRUE)
#   PKG <- srcpkg(path = 'AA')
  
#   ### not loaded yet
#   expect_true(pkg_needs_reload('AA'))

#   ### loaded via srcpkg: 
#   pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE)
#   expect_false(pkg_needs_reload('AA'))

#   # unloaded
#   pkg_unload(PKG, quiet = TRUE)
#   expect_true(pkg_needs_reload('AA'))

#   # loaded, but no roxygen
#   pkg_just_load_pkg(PKG, quiet = TRUE, attach = FALSE)
#   pkg_delete_doc(PKG$path)
#   expect_true(pkg_needs_reload('AA', quiet = TRUE))

#   # unloaded
#   pkg_unload(PKG, quiet = TRUE)
#   expect_true(pkg_needs_reload('AA'))

#   # loaded via load_all --> needs reload
#   devtools::load_all(PKG, quiet = TRUE)
#   expect_true(pkg_needs_reload('AA'))
#   pkg_unload(PKG, quiet = TRUE)

#   # loaded via library
#   expect_false(pkg_needs_reload('base'))
# })
