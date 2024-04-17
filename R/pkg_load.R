# # @param attach          whether the package should be attached to the search path

# #' loads or reloads if needed a source package
# #'
# #' This function will check that all dependent packages are up-to-date, and
# #' document and reload them as needed.
# #'
# #' To be able to properly load a package, its dependent source packages
# #' must be loaded in proper order.
# #' i.e. if A-->B-->C, the load order must be C, B, A
# #'
# #' @inheritParams params

# #'
# #' @return TRUE iff pkg was reloaded
# #' @export
# pkg_load <- function() {}
# #   pkgid,
# #   deps_graph = compute_pkgs_deps_graph(src_pkgs),
# #   attach = TRUE,
# #   quiet = FALSE,
# #   export_all = FALSE,
# #   ...)
# # {
# # }



# does not manage dependencies
# will only load if the package has changed (cf pkg_has_changed() and param force=)
pkg_just_load_pkg <- function(pkgid, force = FALSE, helpers = FALSE, export_all = FALSE, quiet = FALSE, ...) {
  pkg <- as_srcpkg(pkgid)
  pkg_name <- pkg$package

  if (!force && !pkg_needs_reload(pkg_name, quiet = quiet)) return(invisible(FALSE))

  pkg_roxygenise(pkg$path, quiet = TRUE)

  # package has to be (re)loaded, so first unload it (and its ancestors)
  if (pkg_is_loaded(pkg_name)) {
    pkg_unload(pkg_name, quiet = quiet)
  }

  devtools::load_all(pkg$path, export_all = export_all, helpers = helpers, quiet = quiet, ...)

  pkg_write_md5sum(pkg$path)
  store_srcpkg_meta(pkg)

  invisible(TRUE)
}


# determines if a package should be reloaded (is dirty)
pkg_needs_reload <- function(pkg_name, quiet = FALSE) {
  # if not loaded --> trivial
  if (!pkg_is_loaded(pkg_name)) return(TRUE) 

  ### what kind of package it is? source/installed, managed by srcpkgs or devtools?
  srcpkg <- fetch_srcpkg_meta(pkg_name)
  if (!length(srcpkg)) {
    # N.B: not loaded by srcpkgs
    meta_env <- pkgload::dev_meta(pkg_name)
    if (is.null(meta_env)) {
      # assume it is an installed package --> clean
      return(FALSE)
    }

    # it was loaded by devtools::load_all(), but not using srcpkgs
    # --> we have no way of knowing if it is dirty
    # to be on the safe side: assume the worst --> DIRTY
    return(TRUE)
  }

  pkg_has_changed(srcpkg$path, quiet = quiet)
}
