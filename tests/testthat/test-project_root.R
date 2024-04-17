test_that("init_project_root", {
  setup_temp_dir()
  old <- get_project_root(init = FALSE)
  on.exit(set_project_root(old, force = TRUE), add = TRUE)

  # no .git/
  # when not found, use current dir

  expect_identical(basename(init_project_root()), basename(getwd()))

  expect_error(set_project_root('toto'), 'not a directory')
  dir.create('toto')
  expect_error(set_project_root('toto'), NA)
  expect_identical(get_project_root(), 'toto')

  expect_error(set_project_root(NULL), 'not a string')

  ###
  dir.create('subdir')
  setwd('subdir')

  # a .git file, but not a dir!
  writeLines('', '.git')
  # not found, use current dir
  expect_identical(basename(init_project_root()), basename(getwd()))

  # create an actual dir .git
  unlink('.git')
  dir.create('.git')
  expect_identical(basename(init_project_root()), 'subdir')

  expect_identical(basename(init_project_root()), basename(getwd()))

})



test_that("find_project_root", {
  setup_temp_dir()

  # no .git/
  expect_null(find_project_root())

  # a .git file, but not a dir!
  writeLines('', '.git')
  expect_null(find_project_root())

  dir.create('dir1/dir2/dir3', recursive = TRUE)
  dir.create('dir1/dir2/.git')

  root2 <- file.path(getwd(), 'dir1/dir2')
  # N.B: we compare only basenames because of Windows... :(
  expect_identical(basename(find_project_root('dir1/dir2/dir3')), basename(root2))
  expect_identical(basename(find_project_root('dir1/dir2/')), basename(root2))
  expect_null(find_project_root('dir1/'))

  root3 <- file.path(getwd(), 'dir1/dir2/dir3')
  dir.create(file.path(root3, '.git'))
  expect_identical(basename(find_project_root('dir1/dir2/dir3')), basename(root3))
  expect_identical(basename(find_project_root('dir1/dir2/')), basename(root2))

  ### edge cases
  expect_null(find_project_root(NULL))
})


test_that("parent_dir", {
  expect_identical(parent_dir('/'), NULL)
  expect_identical(parent_dir('/A'), '/')
  expect_identical(parent_dir('/A/'), '/')
  expect_identical(parent_dir('/A/B'), '/A')
  expect_identical(parent_dir('/A/B/'), '/A')
  
})

test_that("find_file_upwards", {
  setup_temp_dir()

  # file that should not exist all the way to /
  filename <- 'should_not_exist_1234567890_REALLY'
  expect_null(find_file_upwards(filename))

  ## create the file in a subdir
  dir.create('dir1')
  path <- file.path(getwd(), 'dir1', filename)
  writeLines('toto', path)

  # not found here
  expect_null(find_file_upwards(filename))

  # found in the subdir tho
  expect_identical(basename(find_file_upwards(filename, 'dir1')), filename)

  # create more subdirs
  dir.create('dir1/dir2/dir3', recursive = TRUE)

  expect_identical(basename(find_file_upwards(filename, 'dir1/dir2/dir3')), filename)
  expect_identical(basename(find_file_upwards(filename, 'dir1/dir2/')), filename)
  expect_identical(basename(find_file_upwards(filename, 'dir1')), filename)

})
