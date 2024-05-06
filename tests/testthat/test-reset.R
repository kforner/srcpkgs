test_that("reset/settings", {
  old <- settings()
  print(old)
  setup_temp_dir()

  on.exit(reset(root = old$root, srcpkgs_paths = old$srcpkgs_paths, 
    should_hack_r_loaders = old$hack_r_loaders_installed), add = TRUE)
  src_pkgs <- examples_srcpkgs_basic()

  ### no source packages
  dir.create('empty')

  lst <- reset(root = 'empty')
  expect_identical(lst, list(
    root = "empty", 
    srcpkgs_paths = character(0), 
    hack_r_loaders_installed = FALSE, 
    hack_r_loaders_enabled = FALSE))

  expect_identical(get_project_root(), lst$root)
  expect_identical(get_srcpkgs_paths(),  lst$srcpkgs_paths)
  expect_identical(is_loaders_hack_installed(),  lst$hack_r_loaders_installed)
  expect_identical(is_loaders_hack_enabled(),  lst$hack_r_loaders_enabled)

  expect_identical(lst, settings())

  ### source packages available
  lst <- reset()

  expect_identical(basename(lst$root), basename(getwd()))
  expect_identical(basename(lst$srcpkgs_paths), c('AA', 'BB'))
  expect_true(lst$hack_r_loaders_installed)
  expect_true(lst$hack_r_loaders_enabled)
})