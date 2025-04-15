
ROOT_KEY <- 'ROOT'

# gets the current project root
get_project_root <- function() {
  get_config(ROOT_KEY)
}

# sets the current project root
set_project_root <- function(path, force = FALSE) {
  if (!force && length(path)) {
    stop_unless(length(path) == 1 && is.character(path), 'bad arg path: "%s", not a string', path)
    stop_unless(dir.exists(path), 'bad arg path: "%s", not a directory', path)
  }
  set_config(ROOT_KEY, path) 
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
  if (!file.exists(git)) {
  # if (!dir.exists(git)) {
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
