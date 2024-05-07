
CONFIG_INIT_KEY <- 'SRCPKGS_INIT'

is_initialized <- function() { isTRUE(get_config(CONFIG_INIT_KEY)) }
set_initialized <- function(value = NULL) { set_config(CONFIG_INIT_KEY, value) }

# performs the initialization, via reset() if not already done
init_if_needed <- function() {
  if (is_initialized()) return()
  reset()
  invisible()
}



#' resets the `srcpkgs` settings
#' 
#' With this function, you can reset or set precisely the settings. 
#
#' @inheritParams params
#' @return the settings (cf [settings()]) invisibly
#' @export
#' @examples
#' \dontrun{
#' # reset to appropriate defaults based on your current directory
#' reset()
#' 
#' # explictly set the project root
#' reset(root = 'my/project/dir')
#' 
#' # explictly set the source package paths (very unlikely)
#' reset(srcpkgs_paths = c('pkgs/mypkg1', 'pkgs/mypkg2'))
#' }
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


#' informs about the settings currently used by `srcpkgs`
#' 
#' @return a named list of:
#'   - initialized: whether the settings are initialized (as triggered by [get_srcpkgs()])
#'   - root: the project root
#'   - srcpkgs_paths: the paths of the source packages to manage
#'   - hack_r_loaders_installed: whether the R loaders are hacked
#'   - hack_r_loaders_enabled: whether the R loaded hack is in action (internal use0
#' @export 
settings <- function() {
  list(
    initialized = is_initialized(),
    root = get_project_root(), 
    srcpkgs_paths = get_srcpkgs_paths(),
    hack_r_loaders_installed = is_loaders_hack_installed(), 
    hack_r_loaders_enabled = is_loaders_hack_enabled()
  )
}
