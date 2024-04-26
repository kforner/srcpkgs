add_on_exit <- function(expr, where = parent.frame()) {
  do.call("on.exit", list(substitute(expr), add = TRUE), envir = where)
}

setup_temp_dir <- function(setwd = TRUE, ...) {
  dir <- tempfile(...)
  dir.create(dir, recursive = TRUE)
  old_dir <- NULL
  if (setwd) old_dir <- setwd(dir)

  # on one line because it not seen by the coverage
  cleanup <- bquote({if (.(setwd)) setwd(.(old_dir));unlink(.(dir), recursive = TRUE)})

  do.call(add_on_exit, list(cleanup, parent.frame()))

  invisible(normalizePath(dir))
}

# add_on_exit <- function(expr, where = parent.frame()) {
#   do.call("on.exit", list(substitute(expr), add = TRUE), envir = where)
# }

find_dangling_srcpkgs <- function() {
  df <- fetch_srcpkgs_meta() %||% return(character())
  df <- df[!file.exists(df$path), ]
  intersect(df$package, loadedNamespaces()) 
}

cleanup_dangling_srcpkgs <- function(quiet = TRUE) {
  pkg_names <- find_dangling_srcpkgs() %||% return()
  pkgs <- lapply(pkg_names, fetch_srcpkg_meta)
  src_pkgs <- srcpkgs(pkgs)
  for (pkg_name in pkg_names) pkg_unload(pkg_name, src_pkgs, quiet = quiet)
}
