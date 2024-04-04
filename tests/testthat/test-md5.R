test_that("md5sum_dir", {
  expect_error(md5sum_dir('NON_EXISTENT_DIR'), 'directory does not exist')
  setup_temp_dir()

  ### empty dir
  expect_identical(md5sum_dir('.'), '')

  pkg_create('.', 'mypkg')
  rdir <- 'mypkg/R'

  ### basic
  hash1 <- md5sum_dir(rdir)
  expect_true(is.character(hash1) && length(hash1) == 1)
  expect_true(nzchar(hash1))

  # reproducible
  expect_identical(md5sum_dir(rdir), hash1)

  # add a file
  new_file_path <- file.path(rdir, 'new_file.new')
  cat('hello', file = new_file_path)
  hash2 <- md5sum_dir(rdir)
  expect_false(hash2 == hash1)
  
  # modify that file
  cat('world', file = new_file_path, append = TRUE)
  hash3 <- md5sum_dir(rdir)
  expect_false(hash3 %in% c(hash1, hash2))

  # rewrite the new file
  cat('hello', file = new_file_path)
  expect_identical(md5sum_dir(rdir), hash2)

  # ignore new file --> also test the extra args
  expect_identical(md5sum_dir(rdir, pattern = '\\.R$'), hash1)

  # delete the new file
  file.remove(new_file_path)
  expect_identical(md5sum_dir(rdir), hash1)
})
