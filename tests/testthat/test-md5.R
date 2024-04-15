

test_that("pkg_write_md5sum", {
  setup_temp_dir()
  pkg_create('.', 'mypkg')

  md5sum <- pkg_md5sum('mypkg')

  files <- pkg_list_files('mypkg')
  expect_identical(names(md5sum), files)
  expect_true(all(nzchar(md5sum)))

  ### pkg_read_md5sum
  expect_null(pkg_read_md5sum('./mypkg/'))

  ### pkg_write_m5sum
  md5_path <- pkg_write_md5sum(file.path(getwd(), 'mypkg'))

  expect_identical(read_md5sum_file(md5_path), md5sum)
  expect_identical(pkg_read_md5sum('mypkg'), md5sum)
})


test_that("md5sum_file ", {
  setup_temp_dir()
  pkg_create('.', 'pkg', namespace = TRUE)

  paths <- file.path('pkg', pkg_list_files('pkg'))
  md5sum <- tools::md5sum(paths)
  
  ### write_md5sum_file
  write_md5sum_file(md5sum, 'MD5')

  # read back
  md5sum2 <- read_md5sum_file('MD5')
  expect_identical(md5sum2, md5sum)

  expect_null(find_first_modified_file(paths, md5sum2))
  
  # modify a md5
  x <- md5sum
  x[3] <- x[1]
  expect_identical(find_first_modified_file(paths, x), structure('modified', name = paths[3]))

  # delete a file from md5sums
  x <- md5sum[-2]
  expect_identical(find_first_modified_file(paths, x), structure('added', name = paths[2]))

  # add a file in md5sums
  x <- md5sum
  x['toto'] <- x[1]
  expect_identical(find_first_modified_file(paths, x), structure('deleted', name = 'toto'))
})

