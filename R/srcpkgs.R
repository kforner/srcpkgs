########################################################################
# wraps a list of source package objects (srcpkg/package) as a S3 class
########################################################################

# creates a new "srcpkgs" object from an existing srcpkgs(nonop), or a list of 
# source package-like objects, or a list of source package paths
srcpkgs <- function(
  pkgs = lapply(paths, devtools::as.package), 
  paths = NULL) 
{
  force(paths)
  force(pkgs)
  if (inherits(pkgs, 'srcpkgs')) return(pkgs)
  stop_unless(length(pkgs), 'bad arg "pkgs": must not be empty')
  stop_unless(is.list(pkgs),'bad arg "pkgs", must be a list')
  stop_unless(inherits(pkgs[[1]], 'package'),'bad arg "pkgs", must be a list of package objects')

  # upgrade to srcpkg if needed
  pkgs <- lapply(pkgs, srcpkg)

  # create the srcpkgs object
  names(pkgs) <- get_elements(pkgs, 'package')
  class(pkgs) <- "srcpkgs"

  pkgs
}

# makes sure the input x is a srcpkgs or can be converted to
# accepted: pkgs names, paths, srcpkgs, srcpkg
as_srcpkgs <- function(x, src_pkgs = get_srcpkgs()) {
  if (!length(x)) stop('bad input')
  
  if (inherits(x, "srcpkgs")) return(x)
  if (inherits(x, "srcpkg")) return(srcpkgs(list(x)))

  stop_unless(is.character(x), 'bad arg: not a srcpkg(s) nor character')

  ## pkg names??
  if (all(x %in% names(src_pkgs))) {
    return(subset_s3_list(src_pkgs, x))
  }

  ### assume there are paths
  srcpkgs(paths = x)
}


graph_from_srcpkgs <- function(src_pkgs, imports = TRUE, depends = TRUE, suggests = FALSE) {
  nb <- length(src_pkgs)
  if (nb == 0) return(NULL)
  nodes <- names(src_pkgs)

  types <- character()
  if (imports) types <- append(types, 'imports')
  if (depends) types <- append(types, 'depends')
  if (suggests) types <- append(types, 'suggests')

  .deps <- function(node) {
    deps <- get_srcpkg_dependencies(src_pkgs[[node]])
    intersect(fast_unlist(deps[types]), nodes)
  }
  deps_lst <- lapply(nodes, .deps)
  names(deps_lst) <- nodes

  graph_from_deps(deps_lst)
}

################### S3 methods#############################################################

#' @export
as.data.frame.srcpkgs <- function(x, ...) {
  # convert the package lists to data frame
  rows <- lapply(x, as.data.frame.list, stringsAsFactors = FALSE)
  # keep columns of interest
  rows <- lapply(rows, '[', c('package', 'version', 'path', 'imports', 'depends', 'suggests'))

  # bind rows to a df
  df <- do.call(rbind, rows)
  # for some reason, it is subclassed as "fs_path"
  df$path <- as.character(df$path)

  df
}

#' @export
print.srcpkgs <- function(x, ...) {
  df <- as.data.frame(x)
  print(df, ...)
  invisible(x)
}



