
#' @export
summary.pkg_check <- function(object, ...) {
  df <- data.frame(package = object$package)  
  for (col in c("errors", "warnings", "notes")) {    
    df[[col]] <- length(object[[col]])  
  }  
  df
}

#' tests a package - runs R CMD check 
#'
#' This function will check a source package.
#'
#' @inheritParams params
#' @param error_on              passed to `devtools::check()`
#' @param check_system_clock    if FALSE, disable the `_R_CHECK_SYSTEM_CLOCK_` check. This check
#'                              sometimes fail because of firewalls...
#' @param ...                   passed to `devtools::check()`
#' @return the results as a `pkg_test` object, or NULL if no tests found
#' @importFrom testthat   test_dir
#' @export
#' @examples
#' \dontrun{
#'  pkg_test("mypkg")
#' }
pkg_check <- function(pkgid, src_pkgs = get_srcpkgs(), lib = ".check",  roxygen = TRUE,
  quiet = FALSE, error_on = "error",  check_system_clock = FALSE, ...) 
{  
  force(src_pkgs)
  pkg <- as_srcpkg(pkgid, src_pkgs)

  if (roxygen) pkg_roxygenise(pkg$path, quiet = TRUE)

  tt <- system.time(
    res <- withr::with_envvar(c(`_R_CHECK_SYSTEM_CLOCK_` = as.integer(check_system_clock)), 
      devtools::check(pkg, error_on = error_on, check_dir = lib, quiet = quiet, ...))
  )

  class(res) <- c('pkg_check', class(res)) 
  invisible(res)
}
