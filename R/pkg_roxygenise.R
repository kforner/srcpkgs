#' roxygenize a source package if needed 
#' 
#' - if the package has not changed (based on the md5sum file), does noting
#' - otherwise roxygenise the package using roxygen2::roxygenise
#' - and update and save the new md5sum file
#' 
#' - N.B: has the side-effect of loading the package
#' 
#' @inheritParams params
#' @param force	      if force(d), do not use the md5-based system to detect package changes
#' @param ...         passed to  [devtools::document()]
#'
#' @return if the roxygenation has been performed
#' @keywords internal
pkg_roxygenise <- function(pkg_path, force = FALSE, quiet = FALSE,  ...) {
  if (!force && !pkg_needs_roxygen(pkg_path, quiet = quiet)) 
    return(invisible(FALSE))
  pkg_roxygenise_wrapper(pkg_path, quiet = quiet, ...)
  invisible(TRUE)
}

pkg_roxygenise_wrapper <- function(pkg_path, quiet = FALSE,  ...) {
  wrapper <- if (quiet) suppressMessages else function(...) { force(...) }
  wrapper(devtools::document(pkg_path, quiet = quiet, ...))
  pkg_write_md5sum(pkg_path)
}


# delete the roxygen-generated documentation files
pkg_delete_doc <- function(pkg_path) {
  old <- setwd(pkg_path)
  on.exit(setwd(old), add = TRUE)

  unlink('NAMESPACE')
  unlink('man', recursive = TRUE)
}

# even though a package has not changed, the documentation may never had been generated
pkg_has_no_doc <- function(pkg_path) {
  ### if no NAMESPACE nor man/ dir --> never roxygenized --> TRUE
  # N.B: I do not remember why we test the emptiness. Probably at one point was created empty
  namespace <- file.path(pkg_path, 'NAMESPACE')
  if (!file.exists(namespace) || file_size(namespace) == 0) return(TRUE)
  if (!dir.exists(file.path(pkg_path, 'man'))) return(TRUE)

  FALSE
}

# tests if the roxygen-generated documentation is outdated
pkg_needs_roxygen <- function(pkg_path, quiet = FALSE) {
  pkg_has_no_doc(pkg_path) || pkg_has_changed(pkg_path, quiet = quiet)
}
