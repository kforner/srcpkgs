

# Note: this should be read-only
dummy_srcpkg_path <- function() {
  system.file("extdata/examples/dummy_srcpkg", package = 'srcpkgs')
}

#' installs the dummy srcpkg in a temp location
#'
#' Intended for testing and to write examples
#'
#' @param dest  where to install the dummy srcpkg
#' @return the package as a `srcpkg` object
#' @export
#' @examples
#' pkg <- setup_and_get_dummy_srcpkg()
#' print(pkg)
setup_and_get_dummy_srcpkg <- function(dest = tempfile()) {
  pkg_path <- dummy_srcpkg_path()
  if (!dir.exists(dest)) dir.create(dest)
  file.copy(pkg_path, dest, recursive = TRUE, overwrite = TRUE)

  as_srcpkg(file.path(dest, basename(pkg_path)), src_pkgs = NULL)
}
