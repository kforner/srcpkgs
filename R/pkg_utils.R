
#' creates and populates a R package-like folder programmatically, useful for writing tests
#' 
#' basically a wrapper around `utils::package.skeleton()`
#
#' @param functions a named list of functionsto add to the package
#' @param imports the "imports" dependencies
#' @param depends the "depends" dependencies
#' @param suggests the "suggests" dependencies
#'
#' @return the package instance, invisibly
#' @keywords internal
#' @export
create_test_package <- function(dir, name, functions = list(DUMMY = function() 'DUMMY'),
   imports = NULL, depends = NULL, suggests = NULL) 
{

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # currently package.skeleton is bugged, you can not pass functions as objects
  # so we create a la mano the R source files

  env <- list2env((functions))
  utils::package.skeleton(name = name, path = dir, environment = env, encoding = 'UTF-8')

  ### update description file
  desc <- file.path(dir, name, 'DESCRIPTION')
  nsp <- file.path(dir, name, 'NAMESPACE')
  die_unless(file.exists(desc),  'can not find DESCRIPTION file at "%s"', desc)

  if (!is.null(imports)) {
    .update_description_file(desc, 'Imports', imports)
    .append_lines_to_file(nsp, sprintf('import(%s)', imports))
    roxygen_import_file <- file.path(dir, name, 'R/imports.R')
    .write_roxygen_imports_file(roxygen_import_file, imports)
  }

  if (!is.null(depends)) .update_description_file(desc, 'Depends', depends)
  if (!is.null(suggests)) .update_description_file(desc, 'Suggests', suggests)

  pkg <- new_srcpkg_from_path(file.path(dir, name))

  invisible(pkg)
}
