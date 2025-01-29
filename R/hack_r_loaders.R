

LOADERS_HACK_KEY <- 'LOADERS_HACK'

set_loaders_hack <- function(status) {
  set_config(LOADERS_HACK_KEY, status)
}
is_loaders_hack_enabled <- function() {
  isTRUE(get_config(LOADERS_HACK_KEY))
}

is_loaders_hack_installed <- function() {
  is_traced(library)
}

is_traced <- function(fun) {
  inherits(fun, "functionWithTrace")
}

INHIBIT_ENV_VAR <- 'SRCPKGS.INHIBIT_R_LOADERS_HACK'
INHIBIT_OPTION <- tolower(INHIBIT_ENV_VAR)

inhibit_r_loaders_hack <- function() {
  raw <- get_env(INHIBIT_ENV_VAR, getOption(INHIBIT_OPTION, FALSE))

  #### may be character, numeric or logical
  # more annoying case: "1" and "0"
  if (is.character(raw) && is.na(as.logical(raw)) && !is.na(as.integer(raw))) {
    raw <- as.integer(raw)
  }

  isTRUE(as.logical(raw))
}


#' instruments the R loaders to make them aware of source packages
#'
#' hacks `library()` and `loadNamespace()` using the base R tracer function `trace()`.
#' `library(pkg)` will basically call `pkg_load(pkg)` if the source package `pkg` 
#' is managed by **srcpkgs**
#'
#' N.B: usually you do not need to call that function explicitly. The function is reentrant.
#' 
#' @section Package startup:
#' 
#' At package startup (actually `.OnAttach()`), `hack_r_loaders()` will be automatically called to hack
#' the R loaders, UNLESS this is inhibited via the option `srcpkgs.inhibit_r_loaders_hack` or the 
#' environment variable `SRCPKGS.INHIBIT_R_LOADERS_HACK`. You may set any value like TRUE, "TRUE", 1 or "1".
#'
#' @return no return value, called for side-effects
#' @export
#' @importFrom utils   getFromNamespace
#' @seealso [unhack_r_loaders()]
#' @examples
#' \donttest{
#' # hack library
#' hack_r_loaders()
#' 
#' # unhack
#' unhack_r_loaders()
#' 
#' # prevent automatic hacking when srcpkgs is loaded
#' options(srcpkgs.inhibit_r_loaders_hack=TRUE)
#' # or
#' Sys.setenv(SRCPKGS.INHIBIT_R_LOADERS_HACK="1")
#' library(srcpkgs)
#' }
hack_r_loaders <- function() {
  hack_library()
  hack_loadNamespace()
  set_loaders_hack(TRUE)
  invisible()
}

#' untraces library() and loadNamespace()
#'
#' The function is reentrant.
#' @return no return value, called for side-effects
#' @export
#' @seealso [hack_r_loaders()]
unhack_r_loaders <- function() {
  suppressMessages(untrace(library))
  suppressMessages(untrace(loadNamespace))
  set_loaders_hack(FALSE)
}


hack_library <- function() {
  tracer <- quote({
      # if ('srcpkgs' %in% loadedNamespaces()) { # in case srcpkgs is unloaded after hacking library
        ### fool R CMD check which does not like :::
        get_from_srcpkgs <- function(x) getFromNamespace(x, 'srcpkgs')
        enabled <- get_from_srcpkgs('is_loaders_hack_enabled')()
        if (enabled) {
          set_loaders_hack <- get_from_srcpkgs('set_loaders_hack')
          # N.B: we disable the hack temporarily to avoid recursive calls
          # e.g. library(pkg1) ---> library(depends1), library(depends2)
          # everything we need will be done by this hack. so we do not need to subsequent calls
          # to trigger that hack and its overhead for nothing
    
          set_loaders_hack(FALSE)
          on.exit(set_loaders_hack(TRUE), add = TRUE)

          # figure out the library() function arguments
          if (!missing(package)) {
            if (!character.only)
              pkg <- as.character(substitute(package))
            else
              pkg <- package
          
            src_pkgs <- get_srcpkgs() # may take some I/O time
            src_pkg <- src_pkgs[[pkg]]
            if (length(src_pkg)) {
              ### this is actually a source package that we manage, so we take care of loading it
              pkg_load(src_pkg, src_pkgs, quiet = TRUE)
            }
          }
        }
    # }
  })

  suppressMessages(trace(library, tracer, print = FALSE))
}

hack_loadNamespace <- function() {
  tracer <- quote({
      # if ('srcpkgs' %in% loadedNamespaces()) { # in case srcpkgs is unloaded after hacking library
        ### fool R CMD check which does not like :::
        get_from_srcpkgs <- function(x) getFromNamespace(x, 'srcpkgs')
        enabled <- get_from_srcpkgs('is_loaders_hack_enabled')()
        if (enabled) {
          set_loaders_hack <- get_from_srcpkgs('set_loaders_hack')
          # N.B: we disable the hack temporarily to avoid recursive calls
          # e.g. loadNamespace(pkg1) ---> loadNamespace(imp1), loadNamespace(imp2)
          # everything we need will be done by this hack. so we do not need to subsequent calls
          # to trigger that hack and its overhead for nothing
    
          set_loaders_hack(FALSE)
          on.exit(set_loaders_hack(TRUE), add = TRUE)

          # figure out the loadNamespace() function arguments
          pkg <- as.character(package)[[1L]] # sometimes contain the version as element #2 ?!

          src_pkgs <- get_srcpkgs() # may take som I/O time
          src_pkg <- src_pkgs[[pkg]]
          if (length(src_pkg)) {
            ### this is actually a source package that we manage, so we take care of loading it
            pkg_load(src_pkg, src_pkgs, attach = FALSE, quiet = TRUE)
          }
        }
    # }
  })

  suppressMessages(trace(loadNamespace, tracer, print = FALSE))
}

