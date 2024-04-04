test_that("srcpkgs", {
  setup_temp_dir()

  pkg1 <- pkg_create('.', 'pkg1')
  pkg2 <- pkg_create('.', 'pkg2')
  pkgs <- list(pkg1, pkg2)

  ### from a list of "package"
  packages <- pkgs
  for (i in seq_along(packages)) 
    class(packages[[i]]) <- "package"

  res <- srcpkgs(packages)

  expect_length(res, 2)
  expect_s3_class(res, 'srcpkgs')
  expect_identical(names(res), c("pkg1", "pkg2"))
  expect_identical(res[[1]], pkg1)
  expect_identical(res[[2]], pkg2)

  ### from some srcpkgs
  res2 <- srcpkgs(res)
  expect_identical(res2, res)

  ### from a list of "srcpkg"
  res2 <- srcpkgs(pkgs)
  expect_identical(res2, res)

  ### from paths
  res2 <- srcpkgs(paths = c('pkg1', 'pkg2'))
  expect_identical(res2, res)

  ### edge cases
  expect_error(srcpkgs(NULL), 'empty')
  expect_error(srcpkgs(list()), 'empty')
  expect_error(srcpkgs(1), 'list')
  expect_error(srcpkgs(list(list(1))), 'package')
})


test_that("as.data.frame.srcpkgs", {
  setup_temp_dir()
  pkg1 <- pkg_create('.', 'pkg1')
  pkg2 <- pkg_create('.', 'pkg2')

  res <- find_srcpkgs('.')
  
  df <- as.data.frame(res)

  expect_s3_class(df, 'data.frame')
  expect_equal(nrow(df), 2)
  expect_identical(df$package, c("pkg1", "pkg2"))
  expect_identical(df$path, file.path(getwd(), c("pkg1", "pkg2")))
})


test_that("print.srcpkgs", {
  setup_temp_dir()
  pkg1 <- pkg_create('.', 'pkg1')
  pkg2 <- pkg_create('.', 'pkg2')

  res <- find_srcpkgs('.')
  out <- capture.output(print(res, width = 200))

  expect_length(out, 3)
  expect_match(out[1], "package")
  expect_match(out[2], "pkg1")
})

