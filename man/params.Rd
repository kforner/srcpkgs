% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_params.R
\name{params}
\alias{params}
\title{shared params}
\arguments{
\item{dry_run}{whether not to actually execute any action having side-effects}

\item{lib}{directory where to install and find installed pkgs}

\item{pkg}{a package as a "srcpkg" object}

\item{pkgs}{packages as a "srcpkgs" object}

\item{pkg_name}{the package name, as a character}

\item{pkg_or_name}{a package name or object ("package" or "srcpkg")}

\item{pkgid}{a package name, path or object}

\item{pkgids}{a list of package ids (names, paths or object), or a
srcpkgs object. Also accept a singleton package object}

\item{md5}{the MD5 hash of the source package}

\item{progress}{whether to display a progress bar}

\item{src_pkgs}{a collection of source packages as a \code{srckgs} object.}

\item{srcpkgs_paths}{paths to the source packages folders}

\item{root}{directory from where to search for source packages}

\item{quiet}{whether to be quiet/silent}

\item{test_filter}{a pattern to select the testthat tests to run.
Test files are names test-xxxxx.R where xxxxx is the
test name. Only test files whose name match the pattern
will be run.}

\item{test_parallel}{whether to run the package tests in parallel}
}
\description{
shared params
}
\keyword{internal}
