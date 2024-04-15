# checks if the package has changed since the last pkg_write_md5sum() call
# if no md5sum file exists, return TRUE since we can not know, so we assume it has changed
pkg_has_changed <- function(pkg_path, quiet = FALSE) {
  md5sum <- pkg_read_md5sum(pkg_path) %||% return(TRUE)

  rel_paths <- pkg_list_files(pkg_path)

  old <- setwd(pkg_path)
  on.exit(setwd(old), add = TRUE)

  change <- find_first_modified_file(rel_paths, md5sum)
  if (!length(change)) return(FALSE)

  if (!quiet) {
    file <- attr(change, 'name') # names() refuse to work here for an unknown reason
    cli::cli_text('package {.pkg {pkg_path}} has changed: {.file {file}} was {.str {change}}')
  }

  TRUE
}


# list files really part of the source package, i.e. the files that should be part of the build (
# (R CMD build) tarball
# N.B: by default exclude lots of files, but also make use of .Rbuildignore to exclude more
# N.B: return relative file paths
pkg_list_files <- function(pkg_path) {
  ### exclusion
  exclude <- default_exclude_patterns()

  # use .Rbuildignore
  ignore_file <- file.path(pkg_path, ".Rbuildignore")
  if (file.exists(ignore_file))
    exclude <- append(exclude, readLines(ignore_file, warn = FALSE))
  exclude <- exclude[nzchar(exclude)]

  exclude_pattern <- paste0(paste0('(', exclude, ')'), collapse = '|')

  paths <- dir(pkg_path, recursive = TRUE, full.names = FALSE, no.. = TRUE)
  paths <- grep(exclude_pattern, paths, invert = TRUE, value = TRUE)

  paths
}
