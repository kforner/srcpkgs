
#' @export
summary.pkg_check <- function(object, ...) {
  df <- data.frame(package = object$package)  
  for (col in c("errors", "warnings", "notes")) {    
    df[[col]] <- length(object[[col]])  
  }  
  df
}

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
