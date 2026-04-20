# shared params

shared params

## Arguments

- dry_run:

  whether not to actually execute any action having side-effects

- lib:

  directory where to install and find installed pkgs

- pkg:

  a package as a "srcpkg" object

- pkgs:

  packages as a "srcpkgs" object

- pkg_name:

  the package name, as a character

- pkg_or_name:

  a package name or object ("package" or "srcpkg")

- pkg_path:

  the package path, as a character

- pkgid:

  a package name, path or package object

- pkgids:

  a list of package ids (names, paths or object), or a srcpkgs object.
  Also accept a singleton package object

- md5:

  the MD5 hash of the source package

- progress:

  whether to display a progress bar

- roxygen:

  whether to roxygenize

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- srcpkgs_paths:

  paths to the source packages folders

- root:

  directory from where to search for source packages

- quiet:

  whether to be quiet/silent

- test_filter:

  a pattern to select the testthat tests to run. Test files are names
  test-xxxxx.R where xxxxx is the test name. Only test files whose name
  match the pattern will be run.

- test_parallel:

  whether to run the package tests in parallel
