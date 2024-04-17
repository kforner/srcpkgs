
#' shared params
#'
#' @param deps_graph		a source package dependencies graph, as an igraph object
#' @param lib         	directory where to install and find installed pkgs
#' @param pkg						a package as a "srcpkg" object
#' @param pkgs					packages as a "srcpkgs" object
#' @param pkg_name			the package name, as a character
#' @param pkg_or_name   a package name or object ("package" or "srcpkg")
#' @param pkgid         a package name, path or object
#' @param pkgids        a list of package ids (names, paths or object), or a
#' 											srcpkgs object. Also accept a singleton package object
#' @param md5         	the MD5 hash of the source package
#' @param progress 			whether to display a progress bar
#' @param src_pkgs      a collection of source packages as a `srckgs` object.
#' @param quiet         whether to be quiet/silent
#' @param test_filter		a pattern to select the testthat tests to run.
#'                      Test files are names test-xxxxx.R where xxxxx is the
#'                      test name. Only test files whose name match the pattern
#'                      will be run.
#' @param test_parallel	whether to run the package tests in parallel
#'
#' @name params
#' @keywords internal
#'
NULL
