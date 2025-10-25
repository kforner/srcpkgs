
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
#' @param namespace whether to write the namespace file (currently only applicable to the imports.
#'        N.B: if the namespace file is generated, roxygen will refuse to update it
#' @param roxygen_imports whether to write the roxygen statements to defined the imports
#' @param ignore_hidden_files whether to create a `.Rbuildignore` file to ignore hidden files.
#' 
#' @return the srcpkg instance, invisibly
#' @keywords internal
pkg_create <- function(dir, name, functions = list(dummy = function() 'DUMMY'),
   imports = NULL, depends = NULL, suggests = NULL, namespace = FALSE, roxygen_imports = FALSE,
   ignore_hidden_files = TRUE)
{

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  env <- list2env((functions))
  mute(utils::package.skeleton(name = name, path = dir, environment = env, encoding = 'UTF-8'), 
    warnings = FALSE, output = FALSE)
  

  pkg_path <- file.path(dir, name)

  desc_path <- file.path(pkg_path, 'DESCRIPTION')
  stop_unless(file.exists(desc_path),  'can not find DESCRIPTION file at "%s"', desc_path)

  fix_description(desc_path, list(License = "GPL-3"))

  ns_path <- file.path(pkg_path, 'NAMESPACE')
  if (!namespace) {
    unlink(ns_path)
  }

  .add_section_to_description <- function(section, contents) {
    spacer <- '  '
    line <- paste0(paste0(section, ': \n', spacer), paste0(contents, collapse = paste0(",\n", spacer)), '\n')
    cat(line, file = desc_path, append = TRUE)
  }

  if (!is.null(imports)) {
    .add_section_to_description('Imports', imports)

    if (namespace) {
      cat(sprintf('import(%s)', imports), file = ns_path, sep = '\n', append = TRUE)
    }
    if (roxygen_imports) {
      roxygen_import_file <- file.path(pkg_path, 'R/imports.R')
      lines = c(sprintf("#' @import %s", imports), 'NULL')
      cat(lines, file = roxygen_import_file, sep = '\n')
    }
  }
  if (!is.null(depends)) .add_section_to_description('Depends', depends)
  if (!is.null(suggests)) .add_section_to_description('Suggests', suggests)

  if (ignore_hidden_files) {
    cat("^[.].*$\n^.*/[.].*$\n", file = file.path(pkg_path, ".Rbuildignore"))
  }

  ### man
  unlink(file.path(pkg_path, "man"), recursive = TRUE)

  invisible(srcpkg(path = pkg_path))
}


fix_description <- function(path, lst) {
  df <- read.dcf(path, all = TRUE)
  df2 <- utils::modifyList(df, lst)
  write.dcf(df2, path)
}

# tests if a package is loaded (but maybe not attached)
pkg_is_loaded <- function(pkg_or_name) {
  isNamespaceLoaded(as_pkg_name(pkg_or_name))
}

# tests if a package is attached
pkg_is_attached <- function(pkg_or_name) {
  as_pkg_name(pkg_or_name) %in% pkg_list_attached()
}

#' lists the packages that are attached, i.e. present in the R search() path
#' @export
pkg_list_attached <- function() {
  pattern <- '^package:'
  sub(pattern, '', grep(pattern, search(), value = TRUE))
}

pkg_detach <- function(pkg_or_name) {
  pkg_name <- as_pkg_name(pkg_or_name)
  if (pkg_is_attached(pkg_name)) {
    pn <- paste0('package:', pkg_name)
    detach(pn, character.only = TRUE)
  }
}

# pkg_attach <- function(pkg_name) {
#   if (pkg_is_attached(pkg_name)) return()
#   # we must trick R CMD check, that does not like us messing with package from inside a package
#   get('attach', envir = baseenv())(NULL, name = paste0('package:', pkg_name))
# }

# for a loaded package/namespace pkg, find the packages that are namespace-imported by this package
#  this is fast and do not require any I/O, everything happens in memory
# @return the package names
pkg_list_ns_imports <- function(pkg_name) {
  ns <- getNamespace(pkg_name)
  lst <- getNamespaceImports(ns)
  # currently the output is list with 2 parts:
  # 1. the imported package names
  # 2. the imported functions, as lists named after the package
  pkgs <- unique(names(lst))

  pkgs[nzchar(pkgs)]
}


