library(devtools)
pkg <- as.package('.')
load_all(pkg)

check_man(pkg)

test(pkg, 'config')
test(pkg, 'find')
test(pkg, 'graph')
test(pkg, 'hack')
test(pkg, 'md5')
test(pkg, 'meta')
test(pkg, 'pkg_has_changed')
test(pkg, 'pkg_load')
test(pkg, 'pkg_unload')
test(pkg, 'pkg_utils')
test(pkg, 'plan')
test(pkg, 'reset')
test(pkg, 'root')
test(pkg, 'roxy')
test(pkg, 'srcpkg$')
test(pkg, 'srcpkgs')
test(pkg, '^utils')
test(pkg, 'zzz')

covr::report(covr::package_coverage())
covr::codecov()

library(usethis)


build_vignettes()