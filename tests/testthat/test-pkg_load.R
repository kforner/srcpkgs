E <- list()
R <- list(roxygen = TRUE)
RA <- list(roxygen = TRUE, attach = TRUE)
A <- list(attach = TRUE)

test_that("pkg_load", {
  ### trivial example A->B
  setup_temp_dir()
  src_pkgs <- examples_srcpkgs_basic()
  cleanup_dangling_srcpkgs()
  on.exit(cleanup_dangling_srcpkgs, add = TRUE)
  NONE <- character()
  ALL <- names(src_pkgs)
  unloadNamespace('AA')
  ###
  plan <- pkg_load('AA', src_pkgs, quiet = TRUE)

  expect_equal(nrow(plan), 2)
  expect_true(pkg_is_loaded('AA'))
  expect_true(pkg_is_attached('AA'))
  expect_true(pkg_is_loaded('BB'))
  expect_false(pkg_is_attached('BB'))
  
  ### again
  expect_null(pkg_load('AA', src_pkgs, quiet = TRUE))

  # dirty package BB
  writeLines('#hello', 'BB/R/hello.R')

  plan <- pkg_load('AA', src_pkgs, quiet = TRUE)
  expect_equal(nrow(plan), 4)
})


test_that("load_plan", {
  ### A->C->D, B->D
  mat <- graph_from_strings('A->C->D', 'B->D')
  NONE <- character()
  ALL <- LETTERS
  # N.B: A,B,D,C would have also worked
  plan <- load_plan(c('A', 'B'), mat)
  expect_identical(unique(plan$action), 'load')
  expect_identical(plan$package, c('D', 'B', 'C', 'A'))

  expect_identical(load_plan('A', mat)$package, c('D', 'C', 'A'))
  expect_identical(load_plan('B', mat)$package, c('D', 'B'))
  expect_identical(load_plan('C', mat)$package, c('D', 'C'))
  expect_identical(load_plan('D', mat)$package, 'D')
})


test_that("pkg_load_full_plan() - examples_srcpkgs_complex_deps", {
  setup_temp_dir()
  src_pkgs <- examples_srcpkgs_complex_deps()
  cleanup_dangling_srcpkgs()
  on.exit(cleanup_dangling_srcpkgs, add = TRUE)
  NONE <- character()
  ALL <- names(src_pkgs)

  ##################################################
  ### load A
  # none loaded

  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = NONE, outdated = NONE)

  expect_identical(plan$package, c("EE", "DD", "CC", "BB", "AA"))
  expect_identical(unique(plan$action), 'load')
  expect_identical(plan$params, list(A, E, A, E, A))
  
  # all loaded
  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = ALL, outdated = NONE)
  expect_null(plan)

  # all loaded and outdated
  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = ALL, outdated = ALL)

  expected <- data.frame(
    package = c("AA", "BB", "CC", "DD", "EE", "EE", "DD", "CC", "BB", "AA"), 
    action = c(rep("unload", 5), rep("load", 5)))
  expected$params <- list(E, E, E, E, E, RA, R, RA, R, RA)
  expect_identical(plan, expected)
  
  # all loaded, C outdated
  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = ALL, outdated = 'CC')
  expected <- data.frame(
    package = c("AA", "BB", "CC", "CC", "BB", "AA"), 
    action = c(rep("unload", 3), c("load", "load", "load")))
  expected$params <- list(E, E, E, RA, E, A)
  expect_identical(plan, expected)

  ### load Z
  plan <- pkg_load_full_plan('ZZ', src_pkgs, loaded = NONE, outdated = ALL)
  expected <- data.frame(package = 'ZZ', action = "load")
  expected$params <- list(RA)
  expect_identical(plan, expected)

  ### load B
  plan <- pkg_load_full_plan('BB', src_pkgs, loaded = NONE, outdated = NONE)
  expected <- data.frame(package = c("EE", "DD", "CC", "BB"), action = "load")
  expected$params <- list(A, E, E, A)
  expect_identical(plan, expected)

  # all loaded, E outdated
  plan <- pkg_load_full_plan('BB', src_pkgs, loaded = ALL, outdated = 'EE')
  expected <- data.frame(
    package = c("AA", "BB", "CC", "DD", "EE", "EE", "DD", "CC", "BB", "AA"), 
    action = c(rep("unload", 5), rep("load",  5)))
  expected$params <- list(E, E, E, E, E, RA, E, A, A, E)
  expect_identical(plan, expected)
})

test_that("pkg_load_full_plan() - star example ", {
  setup_temp_dir()
  src_pkgs <- examples_srcpkgs_star()
  cleanup_dangling_srcpkgs()
  on.exit(cleanup_dangling_srcpkgs, add = TRUE)
  NONE <- character()
  ALL <- names(src_pkgs)
  ###############################################################################################
  ### load E
  # none loaded, no suggests
  plan <- pkg_load_full_plan('EE', src_pkgs, loaded = NONE, outdated = ALL)

  # N.B: no 'CC' which is in suggests only
  expect_identical(plan$package, c('AA', 'BB', 'DD', 'EE'))
  expect_identical(unique(plan$action), 'load')
  expect_identical(plan$params,  list(R, RA, RA, RA))

  # none loaded, with suggests
  plan <- pkg_load_full_plan('EE', src_pkgs, loaded = NONE, outdated = ALL, suggests = TRUE)

  expect_identical(plan$package, c('AA', 'BB', 'CC', 'DD', 'EE'))
  expect_identical(unique(plan$action), 'load')
  expect_identical(plan$params,  list(R, RA, R, RA, RA))
  
  # all loaded, none outdated
  plan <- pkg_load_full_plan('EE', src_pkgs, loaded = ALL, outdated = NONE, suggests = TRUE)
  expect_null(plan)

  # all loaded, all outdated
  plan <- pkg_load_full_plan('EE', src_pkgs, loaded = ALL, outdated = ALL, suggests = TRUE)
  expected <- data.frame(
    package = c("EE", "AA", "BB", "CC", "DD", "AA", "BB", "CC", "DD", "EE"),
    action = c(rep("unload", 5), rep("load", 5)))
  expected$params <- list(E, E, E, E, E, R, RA, R, RA, RA)
  expect_identical(plan, expected)

  # all loaded, B outdated
  plan <- pkg_load_full_plan('EE', src_pkgs, loaded = ALL, outdated = 'BB')
  expected <- data.frame(
    package = c("EE",  "BB", "BB", "EE"),
    action = c("unload", "unload", "load",  "load"))
  expected$params <- list(E, E, RA, A)
  expect_identical(plan, expected)

  ### load other, like AA
  # all loaded, none outdated
  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = ALL, outdated = NONE)
  expect_null(plan)

  ### all loaded, AA outdated
  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = ALL, outdated = 'AA')
  expected <- data.frame(
    package = c("EE",  "AA", "AA", "EE"),
    action = c("unload", "unload", "load",  "load"))
  expected$params <- list(E, E, RA, E)
  expect_identical(plan, expected)

  ### all loaded, EE outdated -> nothing to do, E is a parent
  plan <- pkg_load_full_plan('AA', src_pkgs, loaded = ALL, outdated = 'EE')
  expect_null(plan)
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
    action = c("unload", "unload", "unload", "load", "load", "load"))
  expected$params <- list(E, E, E, R, R, RA)
  expect_identical(plan, expected)

  ### all loaded, none outdated
  expect_null(pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = NONE))

  ### all loaded, A outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = 'pkgA')
  expected <- data.frame(package = c("pkgA", "pkgA"), action = c("unload", "load"))
  expected$params <- list(E, RA)
  expect_identical(plan, expected)
  
  ### all loaded, C outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = ALL, outdated = 'pkgC')
  expected <- data.frame(
    package = c("pkgA", "pkgB", "pkgC", "pkgC", "pkgB", "pkgA"),
    action = c("unload", "unload", "unload", "load", "load", "load"))
  expected$params <- list(E, E, E, R, E, A)
  expect_identical(plan, expected)
  
  ### none loaded, none outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = NONE, outdated = NONE)
  expected <- data.frame(package = c("pkgC", "pkgB", "pkgA"), action = "load")
  expected$params <- list(E, E, A)
  expect_identical(plan, expected)
  
  ### none loaded, all outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = NONE, outdated = ALL)
  expected <- data.frame(package = c("pkgC", "pkgB", "pkgA"), action = "load")
  expected$params <- list(R, R, RA)
  expect_identical(plan, expected)

  ### none loaded, C outdated
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = NONE, outdated = 'pkgC')
  expected <- data.frame(package = c("pkgC", "pkgB", "pkgA"), action = "load")
  expected$params <- list(R, E, A)
  expect_identical(plan, expected)

  ### A loaded, B outdated (N.B: that should not be possible)
  plan <- pkg_load_full_plan('pkgA', src_pkgs, loaded = 'pkgA', outdated = 'pkgB')
  expected <- data.frame(package = c("pkgC", "pkgB"), action = "load")
  expected$params <- list(E, R)
  expect_identical(plan, expected)

  ################### load C #################################################
  ### all loaded and outdated
  plan <- pkg_load_full_plan('pkgC', src_pkgs, loaded = ALL, outdated = ALL)
  expected <- data.frame(
    package = c("pkgA", "pkgB", "pkgC", "pkgC", "pkgB", "pkgA"),
    action = c(rep("unload", 3), rep("load", 3)))
  expected$params <- list(E, E, E, RA, R, R)
  expect_identical(plan, expected)

  ### all loaded, only B outdated. B is a parent --> no need to reload C even if its parent is outdated
  expect_null(pkg_load_full_plan('pkgC', src_pkgs, loaded = ALL, outdated = 'pkgB'))

  ### all loaded, C outdated
  plan <- pkg_load_full_plan('pkgC', src_pkgs, loaded = ALL, outdated = 'pkgC')
  expected <- data.frame(
    package = c("pkgA", "pkgB", "pkgC", "pkgC", "pkgB", "pkgA"),
    action = c(rep("unload", 3), rep("load", 3)))
  expected$params <- list(E, E, E, RA, E, E)
  expect_identical(plan, expected)
})



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
