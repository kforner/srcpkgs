#' @section Features:
#'
#' \code{srcpkgs} main objective is to ease development on any project
#' that uses a collection of R source packages (a library).
#' It is able to figure out which dependencies are source packages, and is able
#' to quickly detect changes in any of the used source packages.
#'
#' The main functions are:
#' \describe{
#' 	\item{\code{\link{load_pkg}}}{ find/load/document/reload/unload and reload dependencies
#' 		properly a package }
#'
#'	\item{\code{\link{test_pkg}}}{ test a (list of) package(s),
#' 		possibly running the tests in parallel  }
#' 	\item{\code{\link{check_pkg}}}{ check a (list of) package(s) }
#' 	\item{\code{\link{covr_pkg}}}{ code coverage of a (list of) package(s),
#' 		compatible with parallel tests }
#'	\item{\code{\link{unload_pkg}}}{ unload a package }
#' 	\item{\code{\link{find_srcpkgs}}}{ find the available source packages }
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
