#######################################################################
# wraps a devtools package object into a derived S3 "srcpkg" object
######################################################################

# creates a new "srcpkg" object from a devtools::package, or a path, or a srcpkg (noop)
srcpkg <- function(pkg = devtools::as.package(path), path = NULL, md5 = NA_character_) {
  force(pkg)
  if (inherits(pkg, 'srcpkg')) return(pkg) 
  stop_unless(devtools::is.package(pkg), 'pkg is not a devtools package object')

  pkg$MD5 <- md5
  class(pkg) <- c("srcpkg", "package")

  pkg
}

#' @export
print.srcpkg <- function(x, ...) {
  cli::cli_text("{.pkg {x$package}}@{.url {x$version}} source package [{.file {x$path}}]")
}

# coerces a pkg object or a package name to a package name
# useful to accept multiple input type in a function
as_pkg_name <- function(pkg_or_name) {
  if (is.character(pkg_or_name)) {
    pkg_or_name
  } else {
    stop_unless(devtools::is.package(pkg_or_name), 'bad arg pkg_or_name')
    pkg_or_name$package
  }
}

