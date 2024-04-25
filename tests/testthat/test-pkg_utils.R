
test_that("pkg_create - simple", {
  setup_temp_dir()
  cleanup_dangling_srcpkgs()
  on.exit(cleanup_dangling_srcpkgs(), add = TRUE)

  ### simple (no deps)
  pkg_create('.', 'AA')

  expect_false(pkg_is_loaded('AA'))
  expect_true(file.exists('AA'))

  # can be loaded
  on.exit(unloadNamespace('AA'), add = TRUE)
  devtools::load_all('AA', export_all = FALSE, attach = FALSE, quiet = TRUE)
  expect_true(pkg_is_loaded('AA'))
  expect_false(pkg_is_attached('AA'))

  # inspect some properties
  expect_length(pkg_list_ns_imports('AA'), 0)

  # can be unloaded
  unloadNamespace('AA')
  expect_false(pkg_is_loaded('AA'))

  # attach it
  devtools::load_all('AA', export_all = FALSE, attach = TRUE, quiet = TRUE)
  expect_true(pkg_is_loaded('AA'))
  expect_true(pkg_is_attached('AA'))
})


test_that("pkg_create - imports", {
  setup_temp_dir()

  ### declared imports (i.e. DESCRIPTION-only imports)
  pkg_create('.', 'descimports', imports = c('utils', 'tools'))

  on.exit(unloadNamespace('descimports'), add = TRUE)
  devtools::load_all('descimports', export_all = FALSE, attach = FALSE, quiet = TRUE)

  # N.B: no namespace-imports!!!
  expect_length(pkg_list_ns_imports('descimports'), 0)

  unloadNamespace('descimports')
  expect_error(pkg_list_ns_imports('descimports'), 'no package')

  ### namespace-imports
  pkg_create('.', 'nsimports', imports = c('utils', 'tools'), namespace = TRUE)

  on.exit(unloadNamespace('nsimports'), add = TRUE)
  devtools::load_all('nsimports', export_all = FALSE, attach = FALSE, quiet = TRUE)

  # !! now, the ns-imports are listed
  expect_setequal(pkg_list_ns_imports('nsimports'), c('utils', 'tools'))

  unloadNamespace('nsimports')
  expect_error(pkg_list_ns_imports('nsimports'), 'no package')

  ### namespace-imports via roxygen
  pkg_create('.', 'roxyimports', imports = c('utils', 'tools'), roxygen_imports = TRUE)

  mute(devtools::document('roxyimports', quiet = TRUE))

  on.exit(unloadNamespace('roxyimports'), add = TRUE)
  devtools::load_all('roxyimports', export_all = FALSE, attach = FALSE, quiet = TRUE)

  expect_setequal(pkg_list_ns_imports('roxyimports'), c('utils', 'tools'))

  unloadNamespace('roxyimports')
  expect_error(pkg_list_ns_imports('roxyimports'), 'no package')
})


test_that("pkg_create - depends", {
  setup_temp_dir()

  pkg_create('.', 'AA')
  pkg_create('.', 'BB', depends = 'AA')

  on.exit({
    unloadNamespace('BB')
    unloadNamespace('AA')
  }, add = TRUE)

  # load_all() is not able to load BB
  expect_error(devtools::load_all('BB', export_all = FALSE,  quiet = TRUE), 'required')

  # load AA explicitly and ATTACH it!! Otherwise load_all() will fail
  devtools::load_all('AA', export_all = FALSE, attach = TRUE, quiet = TRUE)

  # then we can load Bb
  devtools::load_all('BB', export_all = FALSE, attach = FALSE, quiet = TRUE)
  expect_true(pkg_is_loaded('BB'))

  # "depends" are not listed as imports!
  expect_length(pkg_list_ns_imports('BB'), 0)
})


# test_that("multiplication works", {
# .create_package <- function() {
#   setup_temp_dir()

#   ###  package with no objects
#   pkg0 <- create_package('.', 'pkg0')
#   expect_true(file.exists('pkg0'))

#   ### normal package
#   unload_pkg('pkg1', quiet = TRUE)
#   pkg1 <- create_package('.', 'pkg1', functions = list(f = function(x,y) x+y))
#   expect_true(file.exists('pkg1'))
#   expect_true(file.exists('pkg1/R'))
#   expect_true(file.exists('pkg1/DESCRIPTION'))

#   expect_is(pkg1, 'package')
#   expect_equal(pkg1$package, 'pkg1')

#   # try to load it
#   devtools::load_all(pkg1, export_all = TRUE)
#   expect_equal( pkg1::f(1,2), 3 )

#   ### package with 1 import)
#   unload_pkg('pkg2', quiet = TRUE)
#   pkg2 <- create_package('.', 'pkg2', imports = 'pkg1')
#   imp2 <- devtools::parse_deps(pkg2$imports)
#   expect_equal(imp2$name, 'pkg1')

#   # check roxygen imports
#   roxygenise_pkg(pkg2, quiet = TRUE)
#   pkg2bis <- as_srcpkg(pkg2$path)

#   deps <- srcpkgs:::compute_srcpkgs_dependencies

#   srcpkgs <- find_srcpkgs('.')
#   expect_identical(deps(pkg2bis, src_pkgs = srcpkgs), deps(pkg2, src_pkgs = srcpkgs))

#   devtools::load_all(pkg2)
#   deps <- srcpkgs:::make_deps_list_of_loaded_pkgs()
#   expect_identical(deps[['pkg2']], 'pkg1')

#   ### package with 2 imports
#   pkg3 <- create_package('.', 'pkg3', imports = c('pkg1', 'pkg2'))
#   imp3 <- devtools::parse_deps(pkg3$imports)
#   expect_equal(imp3$name, c('pkg1', 'pkg2'))

#   ### package with depends and imports and suggests
#   pkg4 <- create_package('.', 'pkg4', imports = 'pkg1', depends = c('pkg3', 'pkg2'), suggests = 'pkg0')

#   imp4 <- devtools::parse_deps(pkg4$imports)
#   expect_equal(imp4$name, 'pkg1')
#   dep4 <- devtools::parse_deps(pkg4$depends)
#   expect_equal(dep4$name, c('pkg3', 'pkg2'))
#   sugg4 <- devtools::parse_deps(pkg4$suggests)
#   expect_equal(sugg4$name, 'pkg0')

#   ### recursive creation of package directory
#   pkg <- create_package('dir1/dir2', 'pkg')
#   expect_true(file.exists('dir1/dir2/pkg/DESCRIPTION'))

#   on.exit({unload_pkg(pkg2, quiet = TRUE); unload_pkg(pkg1, quiet = TRUE)}, add = TRUE)
# }
# test_that('create_package', .create_package())


# })
