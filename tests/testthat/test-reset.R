test_that("reset/settings", {
  OLD <- settings()
  on.exit(restore_init(OLD), add = TRUE)
  src_pkgs <- examples_srcpkgs_basic()

  ### no source packages
  dir.create('empty')

  lst <- reset(root = 'empty')

  expect_identical(basename(lst$root), 'empty')
  expect_identical(basename(lst$srcpkgs_paths), character())

  expect_identical(get_project_root(), lst$root)
  expect_identical(get_srcpkgs_paths(),  lst$srcpkgs_paths)

  expect_identical(lst, settings())

  ### source packages available
  lst <- reset()

  expect_identical(basename(lst$root), basename(getwd()))
  expect_identical(basename(lst$srcpkgs_paths), c('AA', 'BB'))
})