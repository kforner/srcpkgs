
.prune_paths <- function() {
  prune_paths <- srcpkgs:::prune_paths

  expect_identical(prune_paths('AA'), 'AA')
  expect_identical(prune_paths('AA/'), 'AA/')

  expect_identical(prune_paths(c('AA', 'AA/B')), 'AA')
  expect_identical(prune_paths(c('AA/B', 'AA', 'AA/C')), 'AA')
  expect_identical(prune_paths(c('AA/T/CD/B', 'AA', 'BB')), c('AA', 'BB'))
  expect_identical(prune_paths(c('AA', 'BB/A/C')), c('AA', 'BB/A/C'))

  # prefixes
  expect_identical(prune_paths(c('AA', 'AAA')), c('AA', 'AAA'))

  # circular relations
  expect_identical(prune_paths(c('AA', 'AA/B/C', 'AA/B')), 'AA')
}
test_that("prune_paths", {.prune_paths()})


test_that("find_srcpkgs_paths", { 
  find_srcpkgs_paths <- srcpkgs:::find_srcpkgs_paths

  dir <- setup_temp_dir(setwd = FALSE)

  ### edge cases
  # no dir
  expect_error(find_srcpkgs_paths('does_not_exist'), 'bad directory')
  # dir empty
  expect_true(length(find_srcpkgs_paths(dir)) == 0)

  ### A package as level 1
  pkg_create(dir, 'AA')

  pkgs <- find_srcpkgs_paths(dir)

  # N.B: we compare only basenames because of Windows... :(
  expect_identical(basename(pkgs), 'AA')
  expect_identical(pkgs, find_srcpkgs_paths(dir)) # reproducible

  #### A and B in same level 1
  pkg_create(dir, 'BB')

  pkgs <- find_srcpkgs_paths(dir)
  
  expect_identical(pkgs, find_srcpkgs_paths(dir))
  expect_identical(basename(pkgs), c('AA', 'BB'))

  ### C is a level 2 pkg compared to A and B
  pkg_create(file.path(dir, 'C_proj'), 'CC')

  pkgs <- find_srcpkgs_paths(dir)
  
  expect_identical(pkgs, find_srcpkgs_paths(dir))
  expect_setequal(basename(pkgs), c('AA', 'BB', 'CC'))

  ### A1 inside A: should be ignored if prune == TRUE
  pkg_create(file.path(dir, 'AA'), 'AA1')

  pkgs <- find_srcpkgs_paths(dir)

  expect_identical(pkgs, find_srcpkgs_paths(dir))
  expect_setequal(basename(pkgs), c('AA', 'BB', 'CC'))

  pkgs <- find_srcpkgs_paths(dir, prune = FALSE)
  expect_setequal(basename(pkgs), c('AA', 'AA1', 'BB', "CC"))

  ### D inside a hidden directory: should be ignored
  pkg_create(file.path(dir, '.hidden'), 'DD')

  pkgs <- find_srcpkgs_paths(dir)

  expect_identical(pkgs, find_srcpkgs_paths(dir))
  expect_setequal(basename(pkgs), c('AA', 'BB', 'CC'))

  pkgs <- find_srcpkgs_paths(dir, all.files = TRUE)
  expect_true('DD' %in% basename(pkgs))

  ### package name is prefix of other package in same level
  pkg_create(dir, 'AAA')

  pkgs <- find_srcpkgs_paths(dir)
  expect_setequal(basename(pkgs), c('AA', 'AAA', 'BB', "CC"))

  ### should not "find" non-source package ==========================
  lib <- file.path(dir, 'lib')
  dir.create(lib)
  
  # create a source package in lib
  pkg_create(lib, 'installed1')

  # it is found by find_srcpkgs_paths()
  pkgs <- find_srcpkgs_paths(dir)
  expect_true('installed1' %in% basename(pkgs))

  # now turn it to a pseudo installed pakage by creating a Meta/ folder
  dir.create(file.path(lib, 'installed1', 'Meta'))
  expect_length(find_srcpkgs_paths(lib), 0)

  pkgs <- find_srcpkgs_paths(dir)
  expect_false('installed1' %in% basename(pkgs))
})



