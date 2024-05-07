# # @param attach          whether the package should be attached to the search path

#' loads or reloads if needed a source package, taking care of its dependencies
#'
#' N.B: the defaults are different from [devtools::load_all()]: the helpers are not loaded, only 
#' the functions tagged as *exported* are actually exported. The intended goal is to make it as similar
#' to the behaviour of the R loaders.
#' 
#' 
#' This the workhorse function of the package, called by [library()] and [loadNamespace()] 
#' when hacked (cf [hack_r_loaders()].
#' 
#' This function will check that all dependent packages are up-to-date, and
#' document and reload them as needed.
#'
#' To be able to properly load a package, its dependent source packages
#' must be loaded in proper order.
#' i.e. if A-->B-->C, the load order must be C, B, A
#'
#' @inheritParams params
#' @inheritParams pkgload::load_all
#' @inheritDotParams devtools::load_all
#' @param suggests whether to load suggested packages. if TRUE, the suggested are processed
#'    like imports
#' @param roxygen whether to automatically roxygenise packages (if needed)
#' @param ... passed to 
#' @return the load plan as a data frame, or NULL if there is nothing to do.
#' @export
#' @examples
#' \dontrun{
#' # load and attach a package
#' pkg_load('mypkg')
#' 
#' # just load, do not attach it (~ loadNamespace())
#' pkg_load('mypkg', attach = FALSE)
#' 
#' # do some changed, to a source package or any of its depencies or dependents
#' plan <- pkg_load('mypkg', dry_run = TRUE)
#' # then you can inspect the plan actions
#' 
#' 
#' }
pkg_load <- function(pkgid,
  src_pkgs = get_srcpkgs(),
  attach = TRUE,
  suggests = FALSE,
  roxygen = TRUE,
  helpers = FALSE,
  export_all = FALSE, 
  quiet = FALSE,
  dry_run = FALSE,
  ...)
{
  force(src_pkgs)
  pkg <- as_srcpkg(pkgid, src_pkgs)
  pkg_name <- pkg$package
  stop_unless(pkg_name %in% names(src_pkgs), 
    'package "%s" is not managed by srcpkgs, check `get_srcpkgs()`', pkg_name)

  ### store the state of source packages BEFORE
  # df0 <- fetch_srcpkgs_meta()

  plan <- pkg_load_full_plan(pkg_name, src_pkgs, roxygen = roxygen, attach = attach, 
    suggests = suggests, quiet = quiet, ...)

  if (!dry_run) {
     if (length(plan)) 
     execute_plan(plan, src_pkgs, quiet = quiet, helpers = helpers, export_all = export_all)
  }

  invisible(plan)
}


pkg_load_full_plan <- function(pkg_name, src_pkgs, loaded = loadedNamespaces(), 
  outdated = NULL, attach = TRUE, roxygen = TRUE, quiet = FALSE, ...) 
{
  mat <- graph_from_srcpkgs(src_pkgs, ...)
  deps <- graph_get_all_dependencies(mat, pkg_name)
  ordering <- rev(c(pkg_name, deps))

  # find out which packages are outdated
  if (is.null(outdated)) {
    .is_outdated <- function(x) pkg_is_outdated(src_pkgs[[x]]$path, roxygen = roxygen, quiet = quiet)
    outdated <- ordering[sapply(ordering, .is_outdated)]
  }

  # we need to unload: all ordering already loaded AND outdated
  need_unload <- intersect(ordering, intersect(outdated, loaded))
  plana <- NULL
  if (length(need_unload)) plana <- unload_plan(need_unload, mat, loaded = loaded)

  # we need to load:
  # - all unloaded packages (no side-effects), even if not part of ordering
  # - all not loaded packages from ordering
  not_loaded <- setdiff(ordering, loaded)
  pkgs_to_load <- union(plana$package, not_loaded)
  planb <- NULL
  if (length(pkgs_to_load)) {
    planb <- load_plan(pkgs_to_load, mat)
    planb <- planb[planb$package %in% pkgs_to_load, , drop = FALSE]

    # add params
    planb$params <- replicate(nrow(planb), list())

    if (roxygen) {
      idx <- which(planb$package %in% outdated)
      for (i in idx) planb$params[[i]] <- list(roxygen = TRUE)
    }

    ## packages listed as Depends must be attached
    deps_mat <- graph_from_srcpkgs(src_pkgs[planb$package], imports = FALSE, suggests = FALSE)
    to_attach <- colnames(deps_mat)[colSums(deps_mat) > 0]
    if (attach)
      to_attach <- intersect(planb$package, append(to_attach, pkg_name))

    # attach requested package if needed
    if (length(to_attach)) {
      pkg_name_idx <- match(to_attach, planb$package)
      for (i in pkg_name_idx)
        planb$params[[i]] <- append(planb$params[[i]], list(attach = TRUE))
    }
  }

  if (length(plana)) plana$params <- replicate(nrow(plana), list())
  plan <- rbind.data.frame(plana, planb, make.row.names = FALSE)
  if (!nrow(plan)) plan <- NULL

  plan
}

# plan to load pkg_names in the right order inconditionally (even if already loaded)
load_plan <- function(pkg_names, mat) {
  plan <- unload_plan(pkg_names, t(mat), loaded = colnames(mat)) %||% return(NULL)

  plan$action <- 'load'

  plan
}


# return TRUE iff the package should be updated: roxigenised, (re)loaded
pkg_is_outdated <- function(pkg_path, roxygen = TRUE, quiet = FALSE) {
  (roxygen && pkg_has_no_doc(pkg_path)) || pkg_has_changed(pkg_path, quiet)
}

# N.B: should only be called if the package is not loaded or has changed
pkg_load_wrapper <- function(pkg, roxygen = TRUE, attach = FALSE, helpers = FALSE, export_all = FALSE, quiet = FALSE, ...) {
  old_options <- options(rlib_restart_package_not_found = FALSE)
  on.exit(options(old_options), add = TRUE)

  if (roxygen) pkg_roxygenise_wrapper(pkg$path, quiet = quiet)

  devtools::load_all(pkg$path, export_all = export_all, 
    helpers = helpers, quiet = quiet, attach = attach, ...)

  pkg_write_md5sum(pkg$path)
  store_srcpkg_meta(pkg)
}

