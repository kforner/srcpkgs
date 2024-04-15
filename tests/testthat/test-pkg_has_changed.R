test_that("pkg_has_changed", {
  setup_temp_dir()
  pkg_create('.', 'AA')
  pkg_path <- file.path(getwd(), 'AA')

  ### no md5sum
  expect_true(pkg_has_changed(pkg_path))

  ### fresh md5sum
  pkg_write_md5sum(pkg_path)
  expect_false(pkg_has_changed(pkg_path))

  ### add a new file
  new_file_path <- file.path(pkg_path, 'R', 'toto.R')
  writeLines('coucou <- NULL', new_file_path)
  expect_true(pkg_has_changed(pkg_path, quiet = TRUE))

  # refresh
  pkg_write_md5sum(pkg_path)
  expect_false(pkg_has_changed(pkg_path))

  ### modify a file
  writeLines('coucou <- "hello"', new_file_path)
  expect_true(pkg_has_changed(pkg_path, quiet = TRUE))

  # refresh
  pkg_write_md5sum(pkg_path)
  expect_false(pkg_has_changed(pkg_path, quiet = TRUE))

  ### delete a file
  unlink(new_file_path)
  mute(expect_true(pkg_has_changed(pkg_path)))

  # refresh
  md5sum_path <- pkg_write_md5sum(pkg_path)
  expect_false(pkg_has_changed(pkg_path))

  ### delete the md5sum file
  unlink(md5sum_path)
  expect_true(pkg_has_changed(pkg_path))
})


test_that("pkg_list_files", {
  setup_temp_dir()

  pkg_create('.', 'mypkg')

  rel_paths <- pkg_list_files('mypkg')
  paths <- file.path('mypkg', rel_paths)

  common <- c('DESCRIPTION', 'R/dummy.R', 'man/dummy.Rd')
  expect_true(all(file.path('mypkg', common) %in% paths))
  expect_true(all(file.exists(paths)))

  ### ignored files
  .touch <- function(file)  writeLines('', file.path('mypkg', file))
  .present <- function(file) file %in% pkg_list_files('mypkg')
  
  .touch('non_hidden_root_level')
  expect_true(.present('non_hidden_root_level'))

  # add it to the .Rbuildignore file
  cat('^non_hidden_root_level$', file = 'mypkg/.Rbuildignore', append = TRUE)
  # now excluded
  expect_false(.present('non_hidden_root_level'))
  # N.B: .Rbuildignore is also automatically excluded
  expect_false(.present('.Rbuildignore'))

  ### hidden files: automatically excluded
  .touch('.hidden')
  expect_false(.present('.hidden'))
})
