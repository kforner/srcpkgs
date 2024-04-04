
#' creates and populates a R package-like folder programmatically, useful for writing tests
#' 
#' basically a wrapper around `utils::package.skeleton()`
#'
#' @param dir the directory in which to create the package, as a string
#' @param name  the package name, as a string
#' @param functions a named list of functions to add to the package
#' @param imports the "imports" dependencies
#' @param depends the "depends" dependencies
#' @param suggests the "suggests" dependencies
#'
#' @return the srcpkg instance, invisibly
#' @keywords internal
#' @export
pkg_create <- function(dir, name, functions = list(dummy = function() 'DUMMY'),
   imports = NULL, depends = NULL, suggests = NULL) 
{

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  env <- list2env((functions))
  mute(utils::package.skeleton(name = name, path = dir, environment = env, encoding = 'UTF-8'))

  desc_path <- file.path(dir, name, 'DESCRIPTION')
  stop_unless(file.exists(desc_path),  'can not find DESCRIPTION file at "%s"', desc_path)
  ns_path <- file.path(dir, name, 'NAMESPACE')

  .update_description_file <- function(section, contents) {
    section <- paste0(section, ': ')
    line <- paste0(section, paste0(contents, collapse = ","), '\n')
    cat(line, file = desc_path, append = TRUE)
  }

  if (!is.null(imports)) {
    .update_description_file('Imports', imports)
    cat(sprintf('import(%s)', imports), file = ns_path, append = TRUE)
    roxygen_import_file <- file.path(dir, name, 'R/imports.R')
    lines = c(sprintf("#' @import %s", imports), 'NULL')
    cat(lines, file = roxygen_import_file, sep = '\n')
  }
  if (!is.null(depends)) .update_description_file('Depends', depends)
  if (!is.null(suggests)) .update_description_file('Suggests', suggests)

  invisible(srcpkg(path = file.path(dir, name)))
}


# # tests if a package is loaded (but maybe not attached)
# pkg_is_loaded <- function(pkg_or_name) {
#   isNamespaceLoaded(as_pkg_name(pkg_or_name))
# }

# # tests if a package is attached
# pkg_is_attached <- function(pkg_or_name) {
#   as_pkg_name(pkg_or_name) %in% pkg_list_attached()
# }

# # list the packages that are attached, i.e. present in the R search() path
# pkg_list_attached <- function() {
#   pattern <- '^package:'
#   sub(pattern, '', grep(pattern, search(), value = TRUE))
# }
