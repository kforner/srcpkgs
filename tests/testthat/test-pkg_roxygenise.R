test_that("pkg_roxygenise", {
  setup_temp_dir()
  pkg <- pkg_create('.', 'AA')
  src_pkgs <- srcpkgs(list(pkg))
  on.exit(cleanup_dangling_srcpkgs(), add = TRUE)
  unloadNamespace('AA')
  cleanup_dangling_srcpkgs()

  roxy <- "
#' dummy roxy item
#' @param param1 desc1
#' @param param2 desc2
#' @return stuff
#' @export
dummy_function <- function(param1, param2) {}
  "
  writeLines(roxy, 'AA/R/roxy.R')

  ### before
  files <- pkg_list_files('AA')
  expect_true('DESCRIPTION' %in% files)
  expect_false('NAMESPACE' %in% files)

  ### pkg_roxygenise
  expect_false(pkg_is_loaded('AA'))

  expect_true(pkg_roxygenise('AA', quiet = TRUE))

  expect_true('NAMESPACE' %in% pkg_list_files('AA'))
  expect_true('man/dummy_function.Rd' %in% pkg_list_files('AA'))
  # N.B: as a side-effect, the package is now loaded
  expect_true(pkg_is_loaded('AA'))

  ### again
  expect_false(pkg_roxygenise('AA', quiet = TRUE))
  

  ### force regeneration
  cat('# srcpkgs rules!\n', file = 'AA/NAMESPACE', append = TRUE)
  content <- readLines('AA/NAMESPACE')
  expect_match(content, 'srcpkgs', all = FALSE)

  expect_true(pkg_roxygenise('AA', quiet = TRUE, force = TRUE))

  content <- readLines('AA/NAMESPACE')
  expect_no_match(content, 'srcpkgs', all = FALSE)

  ### pkg_delete_doc()
  pkg_delete_doc('AA')

  files <- pkg_list_files('AA')
  expect_false('NAMESPACE' %in% files)
  expect_false('man' %in% unique(dirname(files)))

  ### pkg_roxygenise_wrapper quiet=FALSE
  suppressMessages(pkg_roxygenise_wrapper('AA'))
  expect_true('NAMESPACE' %in% pkg_list_files('AA'))
})






test_that("pkg_needs_roxygen", {
  setup_temp_dir()
  pkg <- pkg_create('.', 'AA')
  src_pkgs <- srcpkgs(list(pkg))
  on.exit(cleanup_dangling_srcpkgs(), add = TRUE)

  refresh <- function() {
    pkg_roxygenise('AA', quiet = TRUE, force = TRUE)
    pkg_needs_roxygen('AA', quiet = TRUE)
  }

  ### never roxygenized
  expect_true(pkg_needs_roxygen('AA', quiet = TRUE))

  ### just been roxygenized
  expect_false(refresh())

  ### remove some files
  unlink('AA/NAMESPACE')
  expect_true(pkg_needs_roxygen('AA', quiet = TRUE))
  expect_false(refresh())

  unlink('AA/man', recursive = TRUE)
  expect_true(pkg_needs_roxygen('AA', quiet = TRUE))
  expect_false(refresh())

  ### add file
  writeLines('one <- 1L', 'AA/R/one.R')
  expect_true(pkg_needs_roxygen('AA', quiet = TRUE))
  expect_false(refresh())

  ### add line to existing file
  cat('### comment\n', file = 'AA/R/one.R', append = TRUE)
  expect_true(pkg_needs_roxygen('AA', quiet = TRUE))
  expect_false(refresh())

  ## delete_pkgs_doc
  pkg_delete_doc('AA')
  expect_true(pkg_needs_roxygen('AA', quiet = TRUE))
  expect_false(refresh())
})