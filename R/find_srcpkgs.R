SRCPKGS_PATHS_KEY <- 'SRCPKGS_PATHS'

#' finds all available source packages starting from the project root
#'
#' N.B: the *hidden* files and directories are ignored.
#' In general, this function is not used directly, instead you should use [get_srcpkgs()]
#' 
#' @inheritParams params
#' @param prune   whether to report packages contained inside another package (e.g. in tests/)
#' @return a "srcpkgs" object
#' @export
find_srcpkgs <- function(root = get_project_root(), 
  srcpkgs_paths = find_srcpkgs_paths(root, prune = prune), prune = TRUE)
{
  srcpkgs(paths = srcpkgs_paths)
}

#' get the current source packages list
#' 
#' The first call to this function will trigger the initialization of the package ((cf [reset()]).
#' Since it is used by mostly all user-facing load-related functions, this enables a runtime initialization, 
#' as opposed to a load-time initialization. So for example
#' you may load `srcpkgs`, then change the current directory to your project. 
#' Then the first load will setup the settings.
#' 
#' For optimization, the paths to discovered source packages are cached (cf [reset()] and [settings()].
#' This function will reparse the DESCRIPTION for any change. 
#' If you add or delete a source package, you must reset the source package paths using [reset()]
#' 
#' This function is useful for troubleshooting, to understand what are the source packages discovered 
#' and managed by `srcpkgs` 
#' 
#' @return the source packages as a "scrpkgs" object, or NULL if none
#' @export
get_srcpkgs <- function() {
  init_if_needed() # triggers initialization
  paths <- get_srcpkgs_paths() %||% return(NULL)

  srcpkgs(paths = paths)
}

# sets the current paths of source packages
set_srcpkgs_paths <- function(paths) { set_config(SRCPKGS_PATHS_KEY, paths) }

get_srcpkgs_paths <- function() {  get_config(SRCPKGS_PATHS_KEY) }


# finds all source packages inside a directory
#by default not in hidden (.) dirs, cf list.files() all.files= param
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



