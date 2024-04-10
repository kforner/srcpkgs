library(devtools)
pkg <- as.package('.')

check_man(pkg)

test(pkg, 'config')
test(pkg, 'dep')
test(pkg, 'find')
test(pkg, 'md5')
test(pkg, 'meta')
test(pkg, 'pkg_utils')
test(pkg, 'root')
test(pkg, 'srcpkg$')
test(pkg, 'srcpkgs')
test(pkg, 'unload')

load_all(pkg)


minimal <- devtools::as.package('tests/testthat/test_library_srcpkgs/minimal')

srcpkg <- srcpkgs:::srcpkg(minimal)
print(srcpkg)

covr::report(covr::package_coverage())

covr::codecov()

library(usethis)
