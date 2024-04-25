PKG_MD5_FILE <- '.SRCPKGS_MD5'


pkg_write_md5sum <- function(pkg_path, md5sum = pkg_md5sum(pkg_path)) {
  force(md5sum)
  md5sum_path <- file.path(pkg_path, PKG_MD5_FILE)
  write_md5sum_file(md5sum, md5sum_path)
  
  invisible(md5sum_path)
}

pkg_read_md5sum <- function(pkg_path) {
  md5sum_path <- file.path(pkg_path, PKG_MD5_FILE)
  if (!file.exists(md5sum_path)) return(NULL)
  read_md5sum_file(md5sum_path)
}


# compute the MD5sum of a source package
pkg_md5sum <- function(pkg_path) {
  paths <- pkg_list_files(pkg_path)

  old <- setwd(pkg_path)
  on.exit(setwd(old), add = TRUE)

  tools::md5sum(paths)
}


pkg_delete_md5sum <- function(pkg_path) {
  md5sum_path <- file.path(pkg_path, PKG_MD5_FILE)
  file.exists(md5sum_path) && unlink(md5sum_path)
}

# write a named character vector (as returned by tools::md5sum) to a MD5 checksum file
write_md5sum_file <- function(md5sum, path) {
  lines <- paste0(md5sum, '  ', names(md5sum))
  writeLines(lines, path)
}

read_md5sum_file <- function(path) {
  lines <- readLines(path)
  tokens <- strsplit(lines, '\\s+')
  mat <- do.call(rbind, tokens)

  structure(mat[, 1], names = mat[, 2])
}

find_first_modified_file <- function(paths, md5sum) {
  paths2 <- names(md5sum)
  
  new <- setdiff(paths, paths2)
  if (length(new)) {
    # there is a new file, not in md5sum
    return(structure('added', name = new[1]))
  }

  deleted <- setdiff(paths2, paths)
  if (length(deleted)) {
    # a file has been deleted, was in md5sum but no longer in paths
    return(structure('deleted', name = deleted[1]))
  }

  # check the md5 file by file
  tools_md5sum <- tools::md5sum
  for (path in paths) {
    if (tools_md5sum(path) != md5sum[path])
      return(structure('modified', name = path))
  }

  NULL
}


# copied from tools:::get_exclude_patterns() since it is not exported
default_exclude_patterns <- function() {
  c(
    "^\\.Rbuildignore$",
    "(^|/)\\.DS_Store$",
    "^\\.(RData|Rhistory)$",
    "~$", "\\.bak$", "\\.swp$",
    "(^|/)\\.#[^/]*$", "(^|/)#[^/]*#$",
    ## Outdated ...
    "^TITLE$", "^data/00Index$",
    "^inst/doc/00Index\\.dcf$",
    ## Autoconf
    "^config\\.(cache|log|status)$",
    "(^|/)autom4te\\.cache$", # ncdf4 had this in subdirectory 'tools'
    ## Windows dependency files
    "^src/.*\\.d$", "^src/Makedeps$",
    ## IRIX, of some vintage
    "^src/so_locations$",
    ## Sweave detrius
    "^inst/doc/Rplots\\.(ps|pdf)$"
    ## Karl: some custom additions
    ,"^Read-and-delete-me"
  )
}
