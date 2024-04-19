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
#' @param ignore_suggests whether not to load suggested packages. if FALSE, the suggested are processed
#'    like imports
#' @param roxygen whether to automatically roxygenise packages (if needed)
#' @param ... passed to 
#' @return FALSE iff not a single package was (re)loaded
#' @export
pkg_load <- function(pkgid,
  src_pkgs = get_srcpkgs(),
  deps_graph = compute_pkgs_dependencies_graph(src_pkgs),
  attach = TRUE,
  ignore_suggests = TRUE,
  force = FALSE, 
  roxygen = TRUE,
  helpers = FALSE,
  export_all = FALSE, 
  quiet = FALSE,
  ...)
{
  force(src_pkgs)
  force(deps_graph)
  pkg <- as_srcpkg(pkgid, src_pkgs)

  ### store the state of source packages BEFORE
  df0 <- fetch_srcpkgs_meta()

  anything_loaded <- pkg_load_dependencies(pkg$package, src_pkgs, deps_graph, 
    ignore_suggests = ignore_suggests, attach = attach, force = force, 
    roxygen = roxygen, helpers = helpers, export_all = export_all, quiet = quiet, ...)

  if (anything_loaded) {
    # restore the state of packages, that may have been unloaded or detached
    df1 <- fetch_srcpkgs_meta()
    pkgs_to_restore <- setdiff(rownames(df0), rownames(df1))
    # process row by row
    for (pkg_name_to_restore in pkgs_to_restore) {
      row  <- df0[pkg_name_to_restore, ]
      Recall(pkg_name_to_restore, src_pkgs, deps_graph, attach = row$attached, quiet = quiet)
    }
  }

  invisible(anything_loaded)
}

# (re)load a source package and its source packages dependencies recursively
# return TRUE iff at least one package has been (re)loaded
pkg_load_dependencies <- function(
  pkg_name,
  src_pkgs,
  deps_graph,
  ignore_suggests,
  quiet,
  attach = TRUE,
  cache = new.env(parent = emptyenv()),
  ...
) {
  ### assumptions
  # pkg_name %in% names(src_pkgs)
  # deps_graph contain src_pkgs

  # check if already processed
  if (exists(pkg_name, envir = cache)) return(FALSE)

  ### compute the package direct source package dependencies
  pkg <- src_pkgs[[pkg_name]] %||% stop_if(TRUE, 'error: package "%s" not in src_pkgs', pkg_name)
  deps_lst <- get_srcpkg_dependencies(pkg)
  src_pkgs_names <- names(src_pkgs)
  for (dep_type in names(deps_lst)) {
    deps <- intersect(src_pkgs_names, deps_lst[[dep_type]])
    # to be extra sure, and avoid cycles, we remove the current package as well
    deps_lst[[dep_type]] <- setdiff(deps, pkg_name)
  }

  ### Depends - N.B: attach = TRUE since Depends are attached to the search path
  depends <- deps_lst[['depends']]
  .load_depend <- function(x, attach = TRUE) {
    pkg_load_dependencies(x, src_pkgs, deps_graph, ignore_suggests = ignore_suggests, 
      quiet = quiet, cache = cache, attach = attach, ...)
  }
  dep_loaded <- vapply(depends, .load_depend, TRUE)
  # fix attachment if needed (packages already loaded and clean but not attached)
  for (dep_pkg_name in setdiff(depends, pkg_list_attached())) 
    pkg_attach(dep_pkg_name)
  
  ### Imports
  # remove depends since they are already processed (above)
  imports <- setdiff(deps_lst[['imports']], depends)
  if (!ignore_suggests) {
      # since suggests are optional, we do not need to reload already loaded suggests
      suggests <- intersect(deps_lst[['suggests']], loadedNamespaces())
      # remove depends from suggests since they are already processed
      suggests <- setdiff(suggests, depends)
      # process suggests as imports
      imports <- union(suggests, imports)
  }
  .load_import <- function(x) .load_depend(x, attach = FALSE)
  imp_loaded <- vapply(imports, .load_import, FALSE) 
  
  ### now that the dependencies are loaded, we can load the package itself
  loaded <- pkg_just_load_pkg(pkg, quiet = quiet, attach = attach, ...)
  # track the loading
  if (loaded) assign(pkg_name, TRUE, cache)
  
  # report if any package was loaded
  any(dep_loaded, imp_loaded, loaded)
}

# does not manage dependencies
# will only load if the package has changed (cf pkg_has_changed() and param force=)
pkg_just_load_pkg <- function(pkgid, force = FALSE, roxygen = TRUE,
  helpers = FALSE, export_all = FALSE, quiet = FALSE, ...) 
{
  pkg <- as_srcpkg(pkgid)
  pkg_name <- pkg$package

  if (!force && !pkg_needs_reload(pkg_name, quiet = quiet)) return(invisible(FALSE))

  if (roxygen) pkg_roxygenise(pkg$path, quiet = TRUE)

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
