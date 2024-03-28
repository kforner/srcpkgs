library(devtools)
pkg <- as.package('.')

check_man(pkg)

test(pkg, 'srcpkg')
test(pkg, 'dep')
load_all(pkg)


minimal <- devtools::as.package('tests/testthat/test_library_srcpkgs/minimal')

srcpkg <- srcpkgs:::srcpkg(minimal)
print(srcpkg)

covr::codecov()
