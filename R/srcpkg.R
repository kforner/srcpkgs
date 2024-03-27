#######################################################################
# wraps a devtools package object into a derived S3 "srcpkg" object
###################################################################

# creates a new "srcpkg" object from a devtools::package
new_srcpkg <- function(pkg) {
  die_unless(devtools::is.package(pkg), 'pkg is not a devtools package object')

  pkg$MD5 <- NA_character_
  class(pkg) <- c("srcpkg", "package")

  pkg
}