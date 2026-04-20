# Package index

## All functions

- [`find_srcpkgs()`](find_srcpkgs.md) : finds all available source
  packages starting from the project root

- [`get_srcpkgs()`](get_srcpkgs.md) : get the current source packages
  list

- [`hack_r_loaders()`](hack_r_loaders.md) : instruments the R loaders to
  make them aware of source packages

- [`pkg_check()`](pkg_check.md) : tests a package - runs R CMD check

- [`pkg_list_attached()`](pkg_list_attached.md) : lists the packages
  that are attached, i.e. present in the R search() path

- [`pkg_load()`](pkg_load.md) : loads or reloads if needed a source
  package, taking care of its dependencies

- [`pkg_roxygenise()`](pkg_roxygenise.md) : roxygenize a source package
  if needed

- [`pkg_test()`](pkg_test.md) : tests a package - runs its unit tests

- [`pkg_unload()`](pkg_unload.md) : unloads a package, unloading its
  dependent packages if needed

- [`pkgs_check()`](pkgs_check.md) : checks a list of source packages

- [`pkgs_deps()`](pkgs_deps.md) : computes the dependencies of some
  (source) packages

- [`pkgs_install()`](pkgs_install.md) : installs a list of source
  packages

- [`pkgs_test()`](pkgs_test.md) : tests a list of source packages

- [`reset()`](reset.md) :

  resets the `srcpkgs` settings

- [`settings()`](settings.md) :

  informs about the settings currently used by `srcpkgs`

- [`setup_and_get_dummy_srcpkg()`](setup_and_get_dummy_srcpkg.md) :
  installs the dummy srcpkg in a temp location

- [`srcpkgs-package`](srcpkgs-package.md) : srcpkgs: R Source Packages
  Manager

- [`srcpkgs()`](srcpkgs.md) : creates a new "srcpkgs" object

- [`unhack_r_loaders()`](unhack_r_loaders.md) : untraces library() and
  loadNamespace()
