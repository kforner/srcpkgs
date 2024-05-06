
CONFIG_INIT_KEY <- 'SRCPKGS_INIT'

is_initialized <- function() { isTRUE(get_config(CONFIG_INIT_KEY)) }
set_initialized <- function(value = NULL) { set_config(CONFIG_INIT_KEY, value) }

# performs the initialization, via reset() if not already done
init_if_needed <- function() {
  if (is_initialized()) return()
  reset()
  invisible()
}



#' reset the srcpkgs settings
#' @inheritParams params
# @param should_hack_r_loaders  whether to hack the R loaders (cf [hack_r_loaders()])
#' @export
reset <- function(
  root = find_project_root(), 
  srcpkgs_paths = find_srcpkgs_paths(root)
  # ,should_hack_r_loaders = should_hack_r_loaders_default(srcpkgs_paths)) 
){
  set_project_root(root)
  set_srcpkgs_paths(srcpkgs_paths)
  # if (should_hack_r_loaders) hack_r_loaders() else unhack_r_loaders()
  set_initialized(TRUE)
  invisible(settings())
}



# should_hack_r_loaders_default <- function(srcpkgs_paths) {
#   (length(srcpkgs_paths) > 0) && (length(find_srcpkgs(srcpkgs_paths = srcpkgs_paths)) > 0)
# }

#' informs about the settings of srcpkgs
#' 
#' @return a named list
#' @export 
settings <- function() {
  list(
    initialized = is_initialized(),
    root = get_project_root(), 
    srcpkgs_paths = get_srcpkgs_paths(),
    hack_r_loaders_installed =  is_loaders_hack_installed(), 
    hack_r_loaders_enabled = is_loaders_hack_enabled()
  )
}
