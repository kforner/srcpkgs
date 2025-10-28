
#' @export
print.pkgs_check <- function(x, ...) {
  ### by package
  df <- as.data.frame(x)
  # only keep ms
  df$time <- format(df$time, digits = 3)
  bad <- which(df$errors > 0)
  print_text_table(df, title = "Check results by package", hilite_rows = bad, heatmap_columns = 'time')

  ### overview
  sdf <- summary(x)
  sdf$time <- format(sdf$time, digits = 3)

  print_text_table(sdf, title = "Check results overview")

  ### overall result (parsable)
  cat('\n')
  ok <- as.logical(x)
  if (ok) {
    cat("SUCCESS\n") 
  } else {
    cat("FAILED\n")
  }

  invisible()
}

#' @export
as.logical.pkgs_check <- function(x, ...) {
   df <- summary(x)
   df$error == 0
}


#' @export
as.data.frame.pkgs_check <- function(x, ...) { 
  do.call(rbind, lapply(x, summary.pkg_check)  )
}

#' @export
summary.pkgs_check <- function(object, ...) {
  df <- as.data.frame(object)

  .sum_rm_na <- function(...) sum(..., na.rm = TRUE)
  df$package <- 1 # N.B: package column is converted to counts
  sdf <- stats::aggregate(df, by = list(.dummy = rep(TRUE, nrow(df))), .sum_rm_na)
  sdf$.dummy <- NULL
 
  sdf
}

#' checks a list of source packages
#'
#' @inheritParams params
#' @param filter          filter out the packages to check using this pattern
#' @param fail_on_error   whether to die if there is at least an error or warning in the checks
#' @param ...   passed to `pkg_check`
#' @return the results as a `pkgs_test` object
#' @export
#' @examples
#' \donttest{
#'  pkg <- setup_and_get_dummy_srcpkg()
#'  res <- pkgs_check(pkg, lib = tempfile(), fail_on_error = FALSE)
#'  print(res)
#' }
pkgs_check <- function(pkgids = names(filter_srcpkgs(src_pkgs, filter)), src_pkgs = get_srcpkgs(), 
  filter = NULL, lib = ".check",  quiet = FALSE, fail_on_error = FALSE,  ...) 
{  
  force(src_pkgs)
  if (!length(pkgids)) { 
    stop('No package to test')
  }
  pkgs <- as_srcpkgs(pkgids, src_pkgs)
  if (!dir.exists(lib))  dir.create(lib, recursive = TRUE)

  if (!quiet) cli::cli_h1('installing packages in {.file lib}')
  pkgs_install(pkgs, lib, src_pkgs = src_pkgs, quiet = TRUE)

  libpath <- c(lib, .libPaths())
  .check_one_pkg <- function(pkg) {   
    if (!quiet) cli::cli_h1('Checking package {.pkg {pkg$package}}')
    # N.B:  roxygen = FALSE because roxygen was already done by pkgs_install()
    pkg_check(pkg, src_pkgs = src_pkgs, lib = lib, roxygen = FALSE, error_on = "never", 
      quiet = quiet, ...)
  }
  tt <- system.time(chks <- withr::with_libpaths(libpath, lapply(pkgs, .check_one_pkg)))

  chks <- new_pkgs_check(chks, names(pkgs))

  summ <- summary(chks)

  if (fail_on_error && (any(summ$errors > 0) || any(summ$warnings > 0))) {
    cli::cli_abort("checks failed")   
  }
  
  invisible(chks)
}

new_pkgs_check <- function(chks, pkgids) {
  names(chks) <- pkgids
  class(chks) <- 'pkgs_check'
  chks
}
