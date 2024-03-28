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

#   deps <- qbdev:::compute_srcpkgs_dependencies

#   srcpkgs <- find_srcpkgs('.')
#   expect_identical(deps(pkg2bis, src_pkgs = srcpkgs), deps(pkg2, src_pkgs = srcpkgs))

#   devtools::load_all(pkg2)
#   deps <- qbdev:::make_deps_list_of_loaded_pkgs()
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
