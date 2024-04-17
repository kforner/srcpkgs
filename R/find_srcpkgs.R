SRCPKGS_KEY <- 'SRCPKGS'

#' find available source packages
#'
#' @param root		from where to search for source packages
#' @param prune   whether to report packages contained inside another package (e.g. in tests/)
#' @return a "srcpkgs" object
#' @export
find_srcpkgs <- function(root = get_project_root(), prune = TRUE) {
  srcpkgs(paths = find_srcpkgs_paths(root, prune = prune))
}

#' get the current source packages list, if any, or discover them using [find_srcpkgs()]
#' @return the source packages as a "scrpkgs" object
#' @export
get_srcpkgs <- function() { 
  src_pkgs <- get_config(SRCPKGS_KEY) %||%  init_srcpkgs()
  # N.B: to get rid of the invisible()
  src_pkgs
}

init_srcpkgs <- function() {
  set_srcpkgs(find_srcpkgs())
}

#' set the current list of source packages
#' 
#' @inheritParams params
#' @return the previous registered value (may be NULL) invisibly
#' @export
set_srcpkgs <- function(src_pkgs) { set_config(SRCPKGS_KEY, src_pkgs) }


#' find all source packages inside a directory
#'
#' by default not in hidden (.) dirs, cf list.files() all.files= param
#'
#' @param dir   			the directory in which to find the pkgs
#' @param src					only report source packages, not installed ones
#' @param	prune				whether to discard pkgs inside other pkgs
#' @param ... 				for \code{\link{list.files}}
#'
#' @return a vector of (relative) package paths
#' @keywords internal
find_srcpkgs_paths <- function(dir, src = TRUE,  prune = TRUE, ...) {
  stop_unless(dir.exists(dir), 'bad directory "%s"', dir)

  # look for all folders with a DESCRIPTION file (required for a R package)
  desc_paths <- dir(dir, pattern = '^DESCRIPTION$', full.names = TRUE, recursive = TRUE, no.. = TRUE, 
    include.dirs = FALSE, ...)
  paths <- dirname(desc_paths)

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
  # add trailing slash to distinguish between dirs and prefixes
  dirs <- paste0(paths, '/')

  .strictly_contained_by <- function(path) {
    which(path != dirs & startsWith(dirs, path))
  }
  ind <- unlist(lapply(dirs, .strictly_contained_by), use.names = FALSE)
  if (length(ind) > 0)
    paths <- paths[-ind]

  paths
}



