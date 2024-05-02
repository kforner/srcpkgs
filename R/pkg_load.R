# # @param attach          whether the package should be attached to the search path

#' loads or reloads if needed a source package, taking care of its dependencies
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
#' @param force  if FALSE, will only reload packages if needed, i.e. if some changes are detected
#' @param suggests whether to load suggested packages. if TRUE, the suggested are processed
#'    like imports
#' @param roxygen whether to automatically roxygenise packages (if needed)
#' @param ... passed to 
#' @return FALSE iff not a single package was (re)loaded
#' @export
pkg_load <- function(pkgid,
  src_pkgs = get_srcpkgs(),
  attach = TRUE,
  suggests = FALSE,
  force = FALSE, 
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

    # attach requested package if needed
    if (attach) {
      pkg_name_idx <- match(pkg_name, planb$package)
      if (!is.na(pkg_name_idx))
        planb$params[[pkg_name_idx]] <- append(planb$params[[pkg_name_idx]], list(attach = TRUE))
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

  if (roxygen) pkg_roxygenise_wrapper(pkg$path, quiet = TRUE)

  # we assume the dependencies are loaded, but not necessarily attached
  # let's take care of the Depends
  depends <- get_srcpkg_dependencies(pkg)$depends
  for (dep in depends) pkg_attach(dep)

  devtools::load_all(pkg$path, export_all = export_all, 
    helpers = helpers, quiet = quiet, attach = attach, ...)

  pkg_write_md5sum(pkg$path)
  store_srcpkg_meta(pkg)
}

