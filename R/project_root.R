
ROOT_KEY <- 'ROOT'
get_project_root <- function() { get_config(ROOT_KEY) }

set_project_root <- function(path) { set_config(ROOT_KEY, path) }

# heuristic: 
# - if a .git/ dir exists in . --> .
# else search upwards for the first folder with a .git/ folder
find_project_root <- function() {

}

