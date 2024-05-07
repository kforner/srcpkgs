# meta data about loaded source packages



# fetches meta data about loaded source packages as a data frame, or NULL if no info
#  N.B: use the devtools meta data, and stores the package hash inside it
fetch_srcpkgs_meta <- function() {
  pkgs_names <- sort(devtools::dev_packages())
  pkgs <- lapply(pkgs_names, fetch_srcpkg_meta)
  pkgs <- pkgs[lengths(pkgs) > 0] %||% return(NULL)
  pkgs <- srcpkgs(pkgs)
  
  df <- as.data.frame(pkgs)

  df$attached <- !is.na(match(rownames(df), sub('package:', '', search())))

  df
}


get_or_set_meta <- function(meta_env, value) {
  key <- 'SRCPKGS_META_KEY'
  if (missing(value)) {
    get0(key, envir = meta_env, inherits = FALSE)
  } else {
    assign(key, value, envir = meta_env)
  }
}

# returns the srcpkg object stored in the devtools meta env of any, or NULL
fetch_srcpkg_meta <- function(pkg_name) {
  if (!pkg_is_loaded(pkg_name)) return(NULL)
  meta_env <- pkgload::dev_meta(pkg_name)
  if (is.null(meta_env)) return(NULL)
  get_or_set_meta(meta_env)
}

# stores the srcpkg, or dies
store_srcpkg_meta <- function(srcpkg) {
  pkg_name <- srcpkg$package
  meta_env <- pkgload::dev_meta(pkg_name)
  stop_if(is.null(meta_env), 'error, no devtools meta env for package "%s"', pkg_name)
  get_or_set_meta(meta_env, srcpkg)
}
