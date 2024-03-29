########################################################################
# wraps a list of source package objects (srcpkg/package) as a S3 class
########################################################################

# creates a new "srcpkgs" object from an existing srcpkgs(nonop), or a list of 
# source package-like objects
srcpkgs <- function(pkgs = lapply(paths, devtools::as.package), paths = NULL) {
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


