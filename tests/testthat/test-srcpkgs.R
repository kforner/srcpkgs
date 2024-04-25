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
  pkg1 <- pkg_create('.', 'pkg1', imports = c('i1', 'i2'), depends = c('d1', 'd2'))
  pkg2 <- pkg_create('.', 'pkg2', imports = 'i1', suggests = c('s1', 's2'))

  res <- find_srcpkgs('.')
  
  df <- as.data.frame(res)

  expect_s3_class(df, 'data.frame')
  expect_equal(nrow(df), 2)
  expect_identical(df$package, c("pkg1", "pkg2"))
  # N.B: on windows the path look something like 
  # C:/Users/runneradmin/AppData/Local/Temp/Rtmpc7Jpvu/working_dir/RtmpoVzues/fileab4454c56a1/pkg1
  # or the short version
  # "C:/Users/RUNNER~1/AppData/Local/Temp/Rtmpc7Jpvu/working_dir/RtmpoVzues/fileab4454c56a1/pkg1
  # --> better not to compare the path, just the basename
  expect_setequal(basename(df$path), c("pkg1", "pkg2"))
  expect_true(all(c('imports', 'depends', 'suggests') %in% names(df)))
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


test_that("graph_from_srcpkgs", {
  setup_temp_dir()

  src_pkgs <- examples_srcpkgs_complex_deps()
  nb <- length(src_pkgs)
  on.exit(cleanup_dangling_srcpkgs(), add = TRUE)

  ### default: imports + depends
  mat <- graph_from_srcpkgs(src_pkgs)

  expect_equal(dim(mat), c(nb, nb))
  .deps <- function(node) names(which(mat[node, ] == 1))
  expect_setequal(.deps('AA'), c('BB', 'CC'))
  expect_setequal(.deps('BB'), c('DD', 'CC'))
  expect_setequal(.deps('CC'), 'DD')
  expect_length(.deps('EE'), 0)
  expect_length(.deps('ZZ'), 0)

  ### only depends
  mat2 <- graph_from_srcpkgs(src_pkgs, imports = FALSE, suggests = FALSE)
  expect_equal(sum(mat2), 2)

  ### only suggests
  mat3 <- graph_from_srcpkgs(src_pkgs, imports = FALSE, depends = FALSE, suggests = TRUE)
  expect_equal(sum(mat3), 1)

  ### edge case
  expect_null(graph_from_srcpkgs(list()))
})
