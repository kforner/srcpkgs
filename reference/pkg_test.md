# tests a package - runs its unit tests

This function will test a source package using `testthat`, making sure
the package and its source package dependencies are up-to-date and
loaded

## Usage

``` r
pkg_test(
  pkgid,
  filter = NULL,
  src_pkgs = get_srcpkgs(),
  export_all = TRUE,
  quiet = TRUE,
  ...
)
```

## Arguments

- pkgid:

  a package name, path or package object

- filter:

  filter in the tests to run. cf
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- export_all:

  passed to [`pkg_load()`](pkg_load.md). Enables the test functions to
  easily access to non-exported functions. Caveat: If the pkg is already
  loaded and up-to-date with export_all=FALSE, it will not work.

- quiet:

  whether to be quiet/silent

- ...:

  passed to
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)

## Value

the results as a `pkg_test` object, which is an empty listL if no tests
were found

## Examples

``` r
 root <- tempfile()
 pkg <- setup_and_get_dummy_srcpkg(root)
 reset(root)
 res <- pkg_test(pkg)
#> ✔ | F W  S  OK | Context
#> 
#> ⠏ |          0 | failure                                                        
#> ✖ | 1        0 | failure
#> ────────────────────────────────────────────────────────────────────────────────
#> Failure (test_failure.R:3:3): does_nothing_special - failure
#> Expected `dummy.srcpkg:::does_nothing_special()` to equal 0.
#> Differences:
#> 1/1 mismatches
#> [1] 42 - 0 == 42
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ⠏ |          0 | success                                                        
#> ✔ |          1 | success
#> 
#> ══ Results ═════════════════════════════════════════════════════════════════════
#> ── Failed tests ────────────────────────────────────────────────────────────────
#> Failure (test_failure.R:3:3): does_nothing_special - failure
#> Expected `dummy.srcpkg:::does_nothing_special()` to equal 0.
#> Differences:
#> 1/1 mismatches
#> [1] 42 - 0 == 42
#> 
#> [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1 ]
#> 
#> No one gets it right on their first try
 print(res)
#> 
#> ── Test results by test for package dummy.srcpkg ───────────────────────────────
#> ╒════════════╤══════════════════════════════╤══╤══════╤══════╤═══════╤═════╤═══════╤═════╕
#> │ file       │ test                         │nb│failed│passed│skipped│error│warning│ time│
#> ╞════════════╪══════════════════════════════╪══╪══════╪══════╪═══════╪═════╪═══════╪═════╡
#> │test_failure│does_nothing_special - failure│ 1│ 1    │ 0    │ FALSE │FALSE│ 0     │0.051│
#> │test_success│does_nothing_special - success│ 1│ 0    │ 1    │ FALSE │FALSE│ 0     │ 0.01│
#> ╘════════════╧══════════════════════════════╧══╧══════╧══════╧═══════╧═════╧═══════╧═════╛
#> 
#> ── Test results by file for package dummy.srcpkg ───────────────────────────────
#> ╒════════════╤══╤══════╤══════╤═══════╤═════╤═══════╤═════╕
#> │ file       │nb│failed│passed│skipped│error│warning│ time│
#> ╞════════════╪══╪══════╪══════╪═══════╪═════╪═══════╪═════╡
#> │test_failure│ 1│ 1    │ 0    │ 0     │ 0   │ 0     │0.051│
#> │test_success│ 1│ 0    │ 1    │ 0     │ 0   │ 0     │ 0.01│
#> ╘════════════╧══╧══════╧══════╧═══════╧═════╧═══════╧═════╛
#> 
#> ── Test results overview for package dummy.srcpkg ──────────────────────────────
#> ╒══╤══════╤══════╤═══════╤═════╤═══════╤══════════════════╕
#> │nb│failed│passed│skipped│error│warning│ time             │
#> ╞══╪══════╪══════╪═══════╪═════╪═══════╪══════════════════╡
#> │ 2│ 1    │ 1    │ 0     │ 0   │ 0     │0.0609999999999982│
#> ╘══╧══════╧══════╧═══════╧═════╧═══════╧══════════════════╛
```
