# add_on_exit <- function(expr, where = parent.frame()) {
#   do.call("on.exit", list(substitute(expr), add = TRUE), envir = where)
# }

# setup_temp_dir <- function(setwd = TRUE, env = parent.frame(), ...) {
#   dir <- tempfile(...)
#   dir.create(dir, recursive = TRUE)
#   old_dir <- NULL
#   if (setwd) old_dir <- setwd(dir)

#   # on one line because it not seen by the coverage
#   cleanup <- bquote({if (.(setwd)) setwd(.(old_dir));unlink(.(dir), recursive = TRUE)})

#   do.call(add_on_exit, list(cleanup, parent.frame()))

#   invisible(normalizePath(dir))
# }
setup_temp_dir <- function(setwd = TRUE, .local_envir = parent.frame(), ...) {
  dir <- withr::local_tempdir(..., .local_envir = .local_envir)
  if (setwd) withr::local_dir(dir, .local_envir = .local_envir)
  invisible(normalizePath(dir))
}

# add_on_exit <- function(expr, where = parent.frame()) {
#   do.call("on.exit", list(substitute(expr), add = TRUE), envir = where)
# }

find_dangling_srcpkgs <- function() {
  pkgs_names <- sort(devtools::dev_packages())
  pkgs <- lapply(pkgs_names, fetch_srcpkg_meta)
  pkgs <- pkgs[lengths(pkgs) > 0] %||% return(NULL)
  pkgs <- srcpkgs(pkgs)
  
  df <- as.data.frame(pkgs)

  df <- fetch_srcpkgs_meta() %||% return(character())
  df <- df[!file.exists(df$path), ]
  intersect(df$package, loadedNamespaces()) 
}

cleanup_dangling_srcpkgs <- function(quiet = TRUE) {
  if (!quiet) message("cleanup_dangling_srcpkgs")
  pkg_names <- find_dangling_srcpkgs() %||% return()
  pkgs <- lapply(pkg_names, fetch_srcpkg_meta)
  src_pkgs <- srcpkgs(pkgs)
  for (pkg_name in pkg_names) pkg_unload(pkg_name, src_pkgs, quiet = quiet)
}

# useful for tests
restore_init <- function(previous) {
  if (previous$initialized) {
    reset(root = previous$root, srcpkgs_paths = previous$srcpkgs_paths)
  } else {
    set_initialized()
    reset(root = NULL, srcpkgs_paths = NULL)
  }
}

fix_pkg_tests_results__timings <- function(res, time = 0) {
  for (i in seq_along(res)) {
    if (is_error(res[[i]])) next
    res[[i]] <- fix_test_result_timings(res[[i]], time)
  }
  res
}

fix_test_result_timings <- function(res, time = 0) {
  for (j in seq_along(res)) {
    res[[j]]$real <- time
  }
  res
}


add_dummy_test_to_srcpkgs <- function(srcpkgs) {
  for (pkg in srcpkgs) add_dummy_test_to_srcpkg(pkg)
}

add_dummy_test_to_srcpkg <- function(srcpkg) {
  withr::local_dir(srcpkg$path)

  dir.create("tests/testthat", recursive = TRUE, showWarnings = FALSE)

  writeLines(r"-----{
  test_that("success", {
    expect_true(TRUE)
  })
  }-----", "tests/testthat/test-success.R")

  writeLines(r"-----{
  test_that("failure", {
    expect_true(FALSE)
  })
  }-----", "tests/testthat/test-failure.R")

  writeLines(r"-----{
  test_that("skip", {
    skip("skipping")
    expect_true(FALSE)
  })
  }-----", "tests/testthat/test-skip.R")

  writeLines(r"-----{
  test_that("mixed", {
    expect_true(FALSE)
    expect_true(TRUE)
  })
  }-----", "tests/testthat/test-mixed.R")

  writeLines(r"-----{
  test_that("stop", {
    expect_true(TRUE)
    stop("Arghh")
    expect_true(TRUE)
  })
  }-----", "tests/testthat/test-error.R")#

  writeLines(r"-----{
  test_that("warn", {
    expect_true(FALSE)
    warning("watch out")
    expect_true(FALSE)
  })
  }-----", "tests/testthat/test-warning.R")#

  writeLines(r"-----{
  test_that("misc1", {
    expect_true(FALSE)
    expect_true(TRUE)
  })
  test_that("misc2", {
    expect_true(FALSE)
    skip("skipping")
  })
  test_that("misc3", {
    expect_true(TRUE)
    expect_true(TRUE)
  })
  test_that("misc4", {
    expect_true(TRUE)
    warning("fais gaffe")
    stop("aie")
    expect_true(TRUE)
  })
  }-----", "tests/testthat/test-misc.R")

  writeLines(sprintf(r"-----{
    library(testthat)
    library(%s)

    test_check("%s")
  }-----", srcpkg$package, srcpkg$package), "tests/testthat.R")
}