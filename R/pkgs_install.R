#' installs a list of source packages
#'
#' - A source package can not be installed if its dependencies are not. 
#' - Will not reinstall packages if they are up-to-date
#' - will roxygenise packages if needed
#
#' @inheritParams params
#' @param only_deps   whether not to include `pkgids`, only their dependencies.
#' @param ...   passed to `devtools::install()`
#' @return the names of the packages actually installed
#' @export
#' @examples 
#' pkg <- setup_and_get_dummy_srcpkg()
#' dest <- tempfile()
#' pkgs_install(pkg, dest)
pkgs_install <- function(pkgids, lib, src_pkgs = get_srcpkgs(), only_deps = FALSE,  quiet = TRUE, ...)
{
  stop_unless(length(pkgids), "No package to test")
  force(src_pkgs)
  pkgs <- as_srcpkgs(pkgids, src_pkgs)

  pkgs_to_install <- pkgs_deps(names(pkgs), src_pkgs, installed = FALSE)
  # N.B: must process them in reverse topological order
  pkgs_to_install <- rev(pkgs_to_install)

  if (!dir.exists(lib)) dir.create(lib, recursive = TRUE)

  .process_pkg <- function(pkg_name) {
    pkg <- src_pkgs[[pkg_name]]

    # is_loaded <- pkg_is_loaded(pkg_name)
    pkg_roxygenise(pkg$path, quiet = quiet)
    # if (!is_loaded) unloadNamespace(pkg_name)
    
    if (!pkg_needs_install(pkg$path, lib)) return(NULL)
    pkg_install_nodeps(pkg$path, lib, quiet = quiet, ...)
    pkg_name
  }
  res <- fast_unlist(lapply(pkgs_to_install, .process_pkg))
  if (!only_deps) {
    res2 <- fast_unlist(lapply(names(pkgs), .process_pkg))
    res <- c(res, res2)
  }

  res
}


# just install the package inconditionnally
# wrapper on devtools::install + store the MD5 sum
# N.B: do not roxygenize, nor modify the source package in any way
pkg_install_nodeps <- function(pkg_path, lib, md5 = pkg_md5sum(pkg_path), quiet = FALSE, quick = TRUE, ...)
{
  stop_unless(is_nz_string(pkg_path), "bad param 'pkg_path', must be a string")
  stop_unless(dir.exists(pkg_path), "pkg_path '%s' does not exist", pkg_path)
  stop_unless(dir.exists(lib), "directory '%s' does not exist", lib)

  pkg <- srcpkg(path = pkg_path)

  libpath <- c(lib, .libPaths())

  withr::with_libpaths(libpath, devtools::install(pkg_path, reload = FALSE, dependencies = FALSE, upgrade = FALSE, quick = quick, quiet = quiet, ...))

  # store the MD5 of the pkg for this installation
  installed_pkg_path <- file.path(lib, pkg$package)
  pkg_write_md5sum(installed_pkg_path, md5)

  invisible(md5)
}

pkg_needs_install <- function(pkg_path, lib, md5 = pkg_md5sum(pkg_path)) {
  pkg <- srcpkg(path = pkg_path)
  installed_pkg_path <- file.path(lib, pkg$package)

  force(md5)
  !identical(pkg_read_md5sum(installed_pkg_path), md5)
}

