# tests a list of source packages

tests a list of source packages

## Usage

``` r
pkgs_test(
  pkgids = names(filter_srcpkgs(src_pkgs, filter)),
  src_pkgs = get_srcpkgs(),
  filter = NULL,
  quiet = TRUE,
  ...
)
```

## Arguments

- pkgids:

  a list of package ids (names, paths or object), or a srcpkgs object.
  Also accept a singleton package object

- src_pkgs:

  a collection of source packages as a `srckgs` object.

- filter:

  filter out the packages to test using this pattern

- quiet:

  whether to be quiet/silent

- ...:

  passed to `pkg_test`

## Value

the results as a `pkgs_test` object

## Examples

``` r
## create a dummy collection of srcpkgs by replicating the dummy srcpkg
 pkg <- setup_and_get_dummy_srcpkg()
 pkgs <- srcpkgs(list(pkg, pkg))

 res <- pkgs_test(pkgs, error_on = "never")
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
#> I believe in you!
 print(res)
#> 
#> ── Test results by package ─────────────────────────────────────────────────────
#> ╒════════════╤══╤══════╤══════╤═══════╤═════╤═══════╤═════╕
#> │ package    │nb│failed│passed│skipped│error│warning│ time│
#> ╞════════════╪══╪══════╪══════╪═══════╪═════╪═══════╪═════╡
#> │dummy.srcpkg│ 2│ 1    │ 1    │ 0     │ 0   │ 0     │0.028│
#> │dummy.srcpkg│ 2│ 1    │ 1    │ 0     │ 0   │ 0     │0.013│
#> ╘════════════╧══╧══════╧══════╧═══════╧═════╧═══════╧═════╛
#> 
#> ── Test results overview ───────────────────────────────────────────────────────
#> ╒═══════╤══╤══════╤══════╤═══════╤═════╤═══════╤═════╕
#> │package│nb│failed│passed│skipped│error│warning│ time│
#> ╞═══════╪══╪══════╪══════╪═══════╪═════╪═══════╪═════╡
#> │ 2     │ 4│ 2    │ 2    │ 0     │ 0   │ 0     │0.041│
#> ╘═══════╧══╧══════╧══════╧═══════╧═════╧═══════╧═════╛
#> 
#> FAILED
```
