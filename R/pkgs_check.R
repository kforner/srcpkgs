
#' @export
print.pkgs_check <- function(x, ...) {
  df <- summary(x)  
  d <- cli::cli_div(theme = list(rule = list(color = "red", "line-type" = "double")))  
  cli::cli_rule("Checks Summary", right = "{df$pkgs} package{?s}")  
  cli::cli_end(d)
  print(df)
}

#' @export
summary.pkgs_check <- function(object, ...) { 
  do.call(rbind, lapply(object, summary.pkg_check)  )
}

#' checks a list of source packages
#'
#' @inheritParams params
#' @param filter          filter out the packages to check using this pattern
#' @param fail_on_error   whether to die if there is at least an error or warning in the checks
#' @param ...   passed to `pkg_check`
#' @return the results as a `pkgs_test` object
#' @export
pkgs_check <- function(pkgids = names(filter_srcpkgs(src_pkgs, filter)), src_pkgs = get_srcpkgs(), 
  filter = NULL, lib = ".check",  quiet = FALSE, fail_on_error = FALSE,  ...) 
{  
  force(src_pkgs)
  if (!length(pkgids)) stop('No package to test')
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
  names(chks) <- names(pkgs)
  class(chks) <- 'pkgs_check'

  summ <- summary(chks)

  if (fail_on_error && (any(summ$errors > 0) || any(summ$warnings > 0))) 
    cli::cli_abort("checks failed")   
  

  invisible(chks)
}
