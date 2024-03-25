

#' find all source packages inside a directory
#'
#' by default not in hidden (.) dirs, cf list.files() all.files= param
#'
#'
#' @param dir   			the directory in which to find the pkgs
#' @param src					only report source packages, not installed ones
#' @param	prune				whether to discard pkgs inside other pkgs
#' @param ... 				for \code{\link{list.files}}
#'
#' @return a vector of (relative) package paths
#' @keywords internal
find_srcpkgs_paths <- function(dir, src = TRUE,  prune = TRUE, ...) {
  stopifnot(dir.exists(dir))

  # look for all folders with a DESCRIPTION file (required for a R package)
  desc <- list.files(dir, pattern = '^DESCRIPTION$', recursive = TRUE, ...)
  paths <- file.path(dir, dirname(desc))

  # remove packages inside other packages (for example in tests/ folder)
  if (prune && length(paths) > 1) {
    paths <- prune_paths(paths)
  }

  if (src) {
    # there could be non-source, i.e. installed packages: remove them
    # Heuristic: installed packages have a Meta/ dir.
    installed <- file.exists(file.path(paths, 'Meta'))
    paths <- paths[!installed]
  }

  unique(sort(paths))
}


prune_paths <- function(paths) {
  # trick to add leading '/' to dirs if missing
  dirs <- file.path(paths, '')

  contained_by <- function(path) {
    which(startsWith(paths, path))
  }
  ind <- unlist(lapply(dirs, contained_by), use.names = FALSE)
  if (length(ind) > 0)
    paths <- paths[-ind]

  paths
}


