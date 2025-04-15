#######################################################################
# wraps a devtools package object into a derived S3 "srcpkg" object
######################################################################

# creates a new "srcpkg" object from a devtools::package, or a path, or a srcpkg (noop)
srcpkg <- function(pkg = devtools::as.package(path), path = NULL) {
  force(pkg)
  if (inherits(pkg, 'srcpkg')) return(pkg) 
  stop_unless(devtools::is.package(pkg), 'pkg is not a devtools package object')

  class(pkg) <- c("srcpkg", "package")

  pkg
}



# makes sure we get a srcpkg instance
as_srcpkg <- function(x, src_pkgs = get_srcpkgs()) {
  stop_unless(length(x), 'bad arg: empty')
  if (inherits(x, 'srcpkg')) return(x)
  if (devtools::is.package(x)) return(srcpkg(x))

  stop_unless(is.character(x), 'bad arg: not an instance nor character')

  # check if it is a srcpkg name from src_pkgs
  pkg <- src_pkgs[[x]]
  if (length(pkg)) return(pkg)

  # assume it is a path
  stop_unless(dir.exists(x), 'bad arg: should be a path, but it does not exist')

  srcpkg(path = x)
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

