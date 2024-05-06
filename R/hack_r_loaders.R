

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

#' instruments the base library() and loadNamespace() to make them aware of source packages
#'
#' hacks library() and loadNamespace() using trace()
#' library(package) will basically call pkg_load(package) if package is managed by srcpkgs
#'
#' @export
#' @seealso unhack_r_loaders
hack_r_loaders <- function() {
  set_loaders_hack(TRUE)
  hack_library()
  hack_loadNamespace()
  invisible()
}

#' untraces library() and loadNamespace()
#'
#' @export
#' @seealso hook_r_loaders
unhack_r_loaders <- function() {
  suppressMessages(untrace(library))
  suppressMessages(untrace(loadNamespace))
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
          
            src_pkgs <- get_srcpkgs() # may take som I/O time
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

