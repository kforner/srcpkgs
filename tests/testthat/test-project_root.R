

test_that("find_project_root", {
  setup_temp_dir()

  # no .git/
  expect_identical(basename(find_project_root()), basename(getwd()))

  # a .git file, but not a dir!
  writeLines('', '.git')
  expect_identical(basename(find_project_root()), basename(getwd()))

  dir.create('dir1/dir2/dir3', recursive = TRUE)
  dir.create('dir1/dir2/.git')
# browser()
  root2 <- file.path(getwd(), 'dir1/dir2')
  # N.B: we compare only basenames because of Windows... :(
  # dir2 because of dir2/.git in ..
  expect_identical(basename(find_project_root('dir1/dir2/dir3')), basename(root2))
  # dir2 because of dir2/.git in .
  expect_identical(basename(find_project_root('dir1/dir2/')), basename(root2))
  # # dir1 by default
  # expect_identical(basename(find_project_root('dir1/')), 'dir1')
  # root because of .git file
 
  expect_identical(basename(find_project_root('dir1/')), basename(getwd()))

  root3 <- file.path(getwd(), 'dir1/dir2/dir3')
  # a .git file, not a directory, like for git submodules
  writeLines("", file.path(root3, '.git'))
  # dir.create(file.path(root3, '.git'))
   # dir3 because of dir3/.git in .
  expect_identical(basename(find_project_root('dir1/dir2/dir3')), basename(root3))
  # dir2 because of dir2/.git in .
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
