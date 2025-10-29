## v 0.2

Got message from Uwe Ligges that the tests and vignettes took too long:

  * checking tests ... [218s] OK
  * checking re-building of vignette outputs ... [339s] OK

It is due to pkg_check() and pkgs_check() that run `R CMD check` that takes at least 5s by package.

Improved that by:

  * skipping long tests on CRAN using `testthat::skip_on_cran()`
  * reducing the number of packages to check in the vignette 

