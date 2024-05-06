
ROOT_KEY <- 'ROOT'

#' get the current project root
#' @param init  whether to automatically define the project root if not defined
#' @return the project root dir path as a string, or NULL if none
#' @export
get_project_root <- function(init = TRUE) { 
  root <- get_config(ROOT_KEY)
  if (init && !length(root))
    root <- init_project_root()
  root
}

#' set the current project root
#' @param path    the project root dir path
#' @param force   whether to bypass sanity checks and set the value
#' @return the previous root as a string, or NULL
#' @export
set_project_root <- function(path, force = FALSE) {
  if (!force) {
    stop_unless(length(path) == 1 && is.character(path), 'bad arg path: "%s", not a string', path)
    stop_unless(dir.exists(path), 'bad arg path: "%s", not a directory', path)
  }
  set_config(ROOT_KEY, path) 
}

#' initializes the project root if needed and checks it
#' 
#' @param root    the project root dir path to set if no root is already set
#' @return the project root invisibly
#' @export
init_project_root <- function(root = find_project_root()) {
  set_project_root(root)
  invisible(root)
}

# heuristic: search upwards for the first folder with a .git/ folder, or return here
find_project_root <- function(here = getwd()) {
  if (!length(here)) return(here)
  root <- here
  git <- find_git_dir(here)

  if (length(git)) 
    root <- parent_dir(git)

  root
}


# N.B: assumes dir is an absolute path to a dir, not a file!!
# this implementation works on all OS (unix and windows)
parent_dir <- function(dir) {
  parent <- dirname(dir)
  if (parent == dir) return(NULL) # will happen on windows. e.g. with C:\
  
  parent
}


find_git_dir <- function(here = getwd()) {
  if (!length(here)) return(NULL)
  git <- find_file_upwards('.git', here)
  if (!length(git)) return(NULL)
  if (!dir.exists(git)) {
    # not a directory, e.g. could be a git submodule symlink. retry from the parent dir if any
    return(find_git_dir(parent_dir(here)))
  }

  git
}

find_file_upwards <- function(filename, folder = getwd()) {
  root <- '/'
  stop_unless(dir.exists(folder), 'directory does not exist: "%s"', folder)
  folder <- normalizePath(folder, '/')

  while(1) {
    path <- file.path(folder, filename)
    if (file.exists(path)) return(path)
    folder <- parent_dir(folder)
    if (is.null(folder)) break # not found
  }

  NULL # not found
}
