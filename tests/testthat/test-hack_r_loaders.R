.untrace <- function(...) {
  suppressMessages(untrace(...))
}

test_that("hack_r_loaders", {

  ### try to restore state
  old <- set_loaders_hack(TRUE)
  on.exit(set_loaders_hack(old), add = TRUE)
  TRACED <- is_traced(library)
  if (TRACED) on.exit(hack_r_loaders(), add = TRUE) else on.exit(unhack_r_loaders(), add = TRUE)

  ### hack_r_loaders
  hack_r_loaders()

  expect_true(is_traced(library))
  expect_true(is_traced(loadNamespace))
  expect_true(is_loaders_hack_enabled())

  ### unhack_r_loaders
  unhack_r_loaders()
  
  expect_false(is_traced(library))
  expect_false(is_traced(loadNamespace))
  expect_true(is_loaders_hack_enabled())

})


test_that("hack_library", {
  setup_temp_dir()
  pkg_name <- 'hack.library'
  pkg_create('.', pkg_name)


  ### try to restore state
  old <- set_loaders_hack(TRUE)
  on.exit(set_loaders_hack(old), add = TRUE)
  TRACED <- is_traced(library)
  if (TRACED) on.exit(hack_library(), add = TRUE) else on.exit(.untrace(library), add = TRUE)
  paths <- find_srcpkgs_paths('.')
  PATHS <- set_srcpkgs_paths(paths)
  on.exit(set_srcpkgs_paths(PATHS), add = TRUE) 

  ### default library() behaviour --> does NOT know about our source packages 
  pkg_unload(pkg_name, quiet = TRUE)
  .untrace(library)

  expect_error(library(hack.library), 'there is no package called')

  ### now hack it
  hack_library()

  expect_error(library(hack.library), NA)
  expect_true(pkg_is_attached(pkg_name))

  ### reentrant
  expect_error(library(pkg_name, character.only = TRUE), NA)

  ### let's modify it a bit: add a new function/symbol
  writeLines('new_fun <- function(x) { x + 1}', file.path(pkg_name, 'R/new_fun.R'))

  expect_error(library(hack.library), NA)
  expect_true(is.function(getFromNamespace('new_fun', pkg_name)))

  ### set_loaders_hack / is_loaders_hack_enabled
  expect_true(is_loaders_hack_enabled())
  set_loaders_hack(FALSE)
  expect_false(is_loaders_hack_enabled())

  # unload
  pkg_unload(pkg_name, quiet = TRUE)
  # suddenly library() does not load our source package anymore
  expect_error(library(pkg_name, character.only = TRUE), 'there is no package called')
})

test_that("hack_loadNamespace", {
  setup_temp_dir()
  pkg_name <- 'hack.loadNamespace'
  pkg_create('.', pkg_name)

  ### try to restore state
  old <- set_loaders_hack(TRUE)
  on.exit(set_loaders_hack(old), add = TRUE)
  TRACED <- is_traced(loadNamespace)
  if (TRACED) on.exit(hack_loadNamespace(), add = TRUE) else on.exit(.untrace(loadNamespace), add = TRUE)
  paths <- find_srcpkgs_paths('.')
  PATHS <- set_srcpkgs_paths(paths)
  on.exit(set_srcpkgs_paths(PATHS), add = TRUE) 

  ### default library() behaviour --> does NOT know about our source packages 
  .untrace(loadNamespace)
  pkg_unload(pkg_name, quiet = TRUE)

  expect_error(loadNamespace(pkg_name), 'there is no package called')

  ### now hack it
  hack_loadNamespace()

  expect_error(loadNamespace(pkg_name), NA)
  
  expect_true(pkg_is_loaded(pkg_name))
  expect_false(pkg_is_attached(pkg_name))
  ### reentrant
  expect_error(loadNamespace(pkg_name), NA)

  ### let's modify it a bit: add a new function/symbol
  writeLines('new_fun <- function(x) { x + 1}', file.path(pkg_name, 'R/new_fun.R'))

  expect_error(loadNamespace(pkg_name), NA)
  expect_true(is.function(getFromNamespace('new_fun', pkg_name)))

  ### unload
  pkg_unload(pkg_name, quiet = TRUE)
  set_loaders_hack(FALSE)
  # suddenly library() does not load our source package anymore
  expect_error(loadNamespace(pkg_name), 'there is no package called')
})


test_that("inhibit_r_loaders_hack", {

  ### try to restore state
  old_env <- get_env(INHIBIT_ENV_VAR)
  old_option <- getOption(INHIBIT_OPTION)
  on.exit({set_env(INHIBIT_ENV_VAR, old_env); set_option(INHIBIT_OPTION, old_option)}, add = TRUE)

  ### defaults
  set_env(INHIBIT_ENV_VAR, '')
  set_option(INHIBIT_OPTION, NULL)

  expect_false(inhibit_r_loaders_hack())

  ### option
  # TRUE
  set_option(INHIBIT_OPTION, 'TRUE')
  expect_true(inhibit_r_loaders_hack())

  set_option(INHIBIT_OPTION, 1)
  expect_true(inhibit_r_loaders_hack())

  # FALSE
  set_option(INHIBIT_OPTION, 'FALSE')
  expect_false(inhibit_r_loaders_hack())

  set_option(INHIBIT_OPTION, 0)
  expect_false(inhibit_r_loaders_hack())

  set_option(INHIBIT_OPTION, '')
  expect_false(inhibit_r_loaders_hack())

  ### env
  set_option(INHIBIT_OPTION, '')
  # TRUE
  set_env(INHIBIT_ENV_VAR, 'TRUE')
  expect_true(inhibit_r_loaders_hack())

  set_env(INHIBIT_ENV_VAR, '1')
  expect_true(inhibit_r_loaders_hack())

  # FALSE
  set_env(INHIBIT_ENV_VAR, 'FALSE')
  expect_false(inhibit_r_loaders_hack())

  set_env(INHIBIT_ENV_VAR, '0')
  expect_false(inhibit_r_loaders_hack())

  set_env(INHIBIT_ENV_VAR, '')
  expect_false(inhibit_r_loaders_hack())
})
