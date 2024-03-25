
.prune_paths <- function() {
  prune_paths <- srcpkgs:::prune_paths

  expect_identical(prune_paths('AA'), 'AA')
  expect_identical(prune_paths('AA/'), 'AA/')

  expect_identical(prune_paths(c('AA', 'AA/B')), 'AA')
  expect_identical(prune_paths(c('AA/B', 'AA', 'AA/C')), 'AA')
  expect_identical(prune_paths(c('AA/T/CD/B', 'AA', 'BB')), c('AA', 'BB'))
  expect_identical(prune_paths(c('AA', 'BB/A/C')), c('AA', 'BB/A/C'))

  # circular relations
  expect_identical(prune_paths(c('AA', 'AA/B/C', 'AA/B')), 'AA')
}
test_that("prune_paths", {.prune_paths()})



# .find_srcpkgs_paths <- function() {
#   find_srcpkgs_paths <- srcpkgs:::find_srcpkgs_paths

#   dir <- setup_temp_dir(chdir = FALSE)



#   ### edge cases
#   # no dir
#   expect_error(find_srcpkgs_paths('does_not_exist'), 'dir.exists')
#   # dir empty
#   expect_true(length(find_srcpkgs_paths(dir)) == 0)


#   ### A package as level 1
#   create_package(dir, 'AA')
#   pkgs <- find_srcpkgs_paths(dir)
#   expect_identical(pkgs, file.path(dir, 'AA'))
#   expect_identical(pkgs, find_src_pkgs_paths(dir))

#   #### A and B in same level 1
#   create_package(dir, 'BB')
#   pkgs <- find_srcpkgs_paths(dir)
#   expect_identical(pkgs, find_src_pkgs_paths(dir))
#   expect_identical(pkgs, file.path(dir, c('AA', 'BB')))


#   ### C is a level 2 pkg compared to A and B
#   create_package(file.path(dir, 'C_proj'), 'CC')
#   pkgs <- find_srcpkgs_paths(dir)
#   expect_identical(pkgs, find_src_pkgs_paths(dir))
#   expect_identical(pkgs, file.path(dir, c('AA', 'BB', "C_proj/CC")))

#   ### A1 inside A: should be ignored if prune == TRUE
#   create_package(file.path(dir, 'AA'), 'AA1')
#   pkgs <- find_srcpkgs_paths(dir)
#   expect_identical(pkgs, find_src_pkgs_paths(dir))

#   expect_identical(pkgs, file.path(dir, c('AA', 'BB', "C_proj/CC")))

#   pkgs <- find_srcpkgs_paths(dir, prune = FALSE)
#   expect_identical(pkgs, sort(file.path(dir, c('AA', 'AA/AA1', 'BB', "C_proj/CC"))))


#   ### D inside a hidden directory: should be ignored
#   create_package(file.path(dir, '.hidden'), 'DD')
#   pkgs <- find_srcpkgs_paths(dir)
#   expect_identical(pkgs, find_src_pkgs_paths(dir))
#   expect_identical(pkgs, file.path(dir, c('AA', 'BB', "C_proj/CC")))

#   pkgs <- find_srcpkgs_paths(dir, all.files = TRUE)
#   expect_true('DD' %in% basename(pkgs))


#   ### package name is prefix of other package in same level
#   create_package(dir, 'AAA')
#   pkgs <- find_srcpkgs_paths(dir)
#   expect_identical(pkgs, file.path(dir, c('AA', 'AAA', 'BB', "C_proj/CC")))

#   ### should not "find" non-source package
#   lib <- file.path(dir, 'lib')
#   dir.create(lib)

#   pkg <- 'testPkg1'
#   .copy_pkg(file.path('test_pkgs', pkg), dir)
#   pkg_path <- file.path(dir, pkg)
#   roxygenise_pkg(pkg_path, quiet = TRUE)
# #  unload_pkg('testPkg1', quiet = TRUE)

#   srcpkgs:::r_cmd_install(pkg_path, lib, quiet = TRUE)

#   pkgs <- find_srcpkgs_paths(lib)
#   expect_equal(length(pkgs), 1)

#   pkgs <- find_src_pkgs_paths(lib)
#   expect_equal(length(pkgs), 0)

# }
# test_that("find_srcpkgs_paths", {.find_srcpkgs_paths()})



# .find_pkgs_path_regression <- function() {
#   find_pkgs_paths <- srcpkgs:::find_pkgs_paths

#   dir <- setup_temp_dir(chdir = FALSE)

#   # make a dummy package with a file containing pattern DESCRIPTION
#   path <- file.path(dir, 'AA')
#   dir.create(path)

#   ### no package
#   expect_length(find_pkgs_paths(dir), 0)

#   ### only one file != DESCRIPTION
#   fname <- file.path(path, 'totoDESCRIPTIONtiti')
#   writeLines('coucou', fname)

#   expect_length(find_pkgs_paths(dir), 0)

#   ### 
#   fname <- file.path(path, 'totodescriptiontiti')
#   expect_length(find_pkgs_paths(dir), 0)
# }
# test_that("find_pkgs_path_regression", .find_pkgs_path_regression())




