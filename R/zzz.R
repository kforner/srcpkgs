# this function is automatically called when the package is attached
# we use that to hack the R loaders unless inhibit_r_loaders_hack()
.onAttach <- function(libname, pkgname) {
  if (!inhibit_r_loaders_hack() && !is_loaders_hack_installed()) {
    srcpkgs::hack_r_loaders()
    packageStartupMessage('hacked R loaders (cf srcpkgs::hack_r_loaders()).')
  }
}

.onDetach <- function(libpath) {
  srcpkgs::unhack_r_loaders()
}
