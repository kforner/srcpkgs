# why would you need srcpkgs?

I will demonstrate srcpkgs using a dummy collection of source packages:
<https://github.com/kforner/srcpkgs_lotr_demo>

## overview of the srcpkgs_lotr_demo collection

It consists currently in 11 related packages, with a tree structure:

- lotr - the highest level package
  - elves
    - legolas
    - galadriel
  - hobbits
    - bilbo
    - frodo
  - gimli
  - aragorn
  - gandalf
  - elrond

N.B: `lotr` depends on all other packages, except for `elrond` (not
yet).

The dependencies are implemented by a mix of Imports, Imports with
namespace imports and Depends.

## using devtools

``` r
suppressPackageStartupMessages(library(devtools))
pkgs <- c('bilbo', 'frodo', 'hobbits', 'legolas', 'galadriel', 'elves', 'gimli', 'aragorn', 'gandalf', 'lotr')
```

### loading

`devtools` is designed to manage a single source package. Let’s use it
to load our `lotr` source package:

``` r
load_all('lotr')
```

    ## ℹ Loading lotr

    ## Error in `load_depends()`:
    ## ! The package "hobbits" is required.

–\> devtools can NOT load `lotr` since it can not possibly find the
`hobbits` package, which is a dependency.

Let’s help him:

``` r
load_all('hobbits')
```

    ## ℹ Loading hobbits

    ## Error in `load_depends()`:
    ## ! The package "bilbo" is required.

–\> same problem

Here is how we must load the packages, following the dependencies order.
Note that we also need to roxygenize them (using
[`document()`](https://devtools.r-lib.org/reference/document.html))

``` r
document('frodo')
load_all('frodo')
document('bilbo')
load_all('frodo')
document('hobbits')
load_all('hobbits')

document('legolas')
load_all('legolas')
document('galadriel')
load_all('galadriel')
document('elves')
load_all('elves')

document('gimli')
load_all('gimli')
document('aragorn')
load_all('aragorn')
document('gandalf')
load_all('gandalf')
```

and finally we can load it

``` r
document('lotr')
```

    ## ℹ Updating lotr documentation
    ## ℹ Setting RoxygenNote to "7.3.3"
    ## ℹ Loading lotr
    ## Writing NAMESPACE
    ## Writing lotr.Rd

``` r
load_all('lotr')
```

    ## ℹ Loading lotr

``` r
# use it
str(lotr())
```

    ## List of 5
    ##  $ hobbits:List of 2
    ##   ..$ frodo:List of 4
    ##   .. ..$ first  : chr "Frodo"
    ##   .. ..$ last   : chr "Baggins"
    ##   .. ..$ weapons: chr [1:2] "sting" "barrow-blade"
    ##   .. ..$ race   : chr "hobbit"
    ##   ..$ bilbo:List of 4
    ##   .. ..$ first  : chr "Bilbo"
    ##   .. ..$ last   : chr "Baggins"
    ##   .. ..$ weapons: chr "sting"
    ##   .. ..$ race   : chr "hobbit"
    ##  $ elves  :List of 2
    ##   ..$ galadriel:List of 4
    ##   .. ..$ first  : chr "Galadriel"
    ##   .. ..$ last   : chr NA
    ##   .. ..$ weapons: chr "nenya"
    ##   .. ..$ race   : chr "elf"
    ##   ..$ legolas  :List of 4
    ##   .. ..$ first  : chr "Legolas"
    ##   .. ..$ last   : chr "Greenleaf"
    ##   .. ..$ weapons: chr "nebownya"
    ##   .. ..$ race   : chr "elf"
    ##  $ dwarves:List of 1
    ##   ..$ gimli:List of 4
    ##   .. ..$ first  : chr "Gimli"
    ##   .. ..$ last   : chr "Durin"
    ##   .. ..$ weapons: chr "axe"
    ##   .. ..$ race   : chr "dwarf"
    ##  $ humans :List of 1
    ##   ..$ aragorn:List of 4
    ##   .. ..$ first  : chr "Aragorn"
    ##   .. ..$ last   : chr "Strider"
    ##   .. ..$ weapons: chr "anduril"
    ##   .. ..$ race   : chr "human"
    ##  $ ainur  :List of 1
    ##   ..$ gandalf:List of 4
    ##   .. ..$ first  : chr "Gandalf"
    ##   .. ..$ last   : chr "Mithrandir"
    ##   .. ..$ weapons: chr "glamdring"
    ##   .. ..$ race   : chr "ainur"

### editing and reloading

Let’s modify one of the direct dependency of `lotr`, e.g. the `hobbits`
package. Currently:

``` r
names(lotr()$hobbits)
```

    ## [1] "frodo" "bilbo"

``` r
lines <- readLines('hobbits/R/main.R')
cat(lines, sep = '\n')
```

    ## #' list the available hobbits
    ## #' @return hobbit names as character
    ## #' @export
    ## hobbits <- function() {
    ##   res <- list()
    ##   # N.B: easier to edit programmatically in the vignette this way
    ##   res$frodo <- frodo::frodo() # in imports
    ##   res$bilbo <- bilbo() 
    ## 
    ##   res
    ## }

Edit `hobbits/R/main.R` and comment out bilbo, which comes from the
`bilbo` package in **Depends**.

``` r
edited_lines <- grep('bilbo', lines, invert = TRUE, value = TRUE)
writeLines(edited_lines, 'hobbits/R/main.R')
```

Let’s try to apply our changes:

``` r
load_all('lotr')
```

    ## ℹ Loading lotr

``` r
names(lotr()$hobbits)
```

    ## [1] "frodo" "bilbo"

–\> [`load_all()`](https://devtools.r-lib.org/reference/load_all.html)
can not properly reload our package, since it does not know that a
dependency has been modified.

To apply the changes:

``` r
load_all('hobbits')
```

    ## ℹ Loading hobbits

``` r
names(lotr()$hobbits)
```

    ## [1] "frodo"

–\> now it works. Note that this is because
[`load_all()`](https://devtools.r-lib.org/reference/load_all.html) is
now able to force the unload of a package even though it is needed by
another package:

``` r
unloadNamespace('hobbits')
```

    ## Error:
    ## ! package 'hobbits' is required by 'lotr' so will not be detached

But:

``` r
devtools::unload('hobbits')
```

Note that now `lotr` is **broken**:

``` r
lotr()
```

    ## Error in `hobbits()`:
    ## ! could not find function "hobbits"

Let’s fix it

``` r
load_all('hobbits')
```

    ## ℹ Loading hobbits

``` r
names(lotr())
```

    ## [1] "hobbits" "elves"   "dwarves" "humans"  "ainur"

## using srcpkgs

``` r
library(srcpkgs)
```

    ## hacked R loaders (cf srcpkgs::hack_r_loaders()).

``` r
old <- options(width = 200)
print(get_srcpkgs())
```

    ##             package version                                                         path                     imports depends suggests
    ## aragorn     aragorn     0.1   /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/aragorn                                             
    ## bilbo         bilbo     0.1     /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/bilbo                                             
    ## elrond       elrond     0.1    /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/elrond                                             
    ## elves         elves     0.1     /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/elves           galadriel,legolas                 
    ## frodo         frodo     0.1     /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/frodo                                             
    ## galadriel galadriel     0.1 /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/galadriel                                             
    ## gandalf     gandalf     0.1   /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/gandalf                                             
    ## gimli         gimli     0.1     /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/gimli                                             
    ## hobbits     hobbits     0.1   /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/hobbits                       frodo   bilbo         
    ## legolas     legolas     0.1   /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/legolas                                             
    ## lotr           lotr     0.1      /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/lotr elves,gimli,aragorn,gandalf hobbits

### unloading

the [`srcpkgs::pkg_unload()`](../reference/pkg_unload.md) takes into
account the dependencies between the source packages.

``` r
plan <- pkg_unload('bilbo')
```

    ## executing unload on lotr

    ## executing unload on hobbits

    ## executing unload on bilbo

``` r
print(plan)
```

    ##   package action
    ## 1    lotr unload
    ## 2 hobbits unload
    ## 3   bilbo unload

unload all our packages to start from a clean state. We will also

``` r
for (pkg in get_srcpkgs()) pkg_unload(pkg)
```

    ## executing unload on aragorn

    ## executing unload on elves

    ## executing unload on frodo

    ## executing unload on galadriel

    ## executing unload on gandalf

    ## executing unload on gimli

    ## executing unload on legolas

### loading

[`srcpkgs::pkg_load()`](../reference/pkg_load.md) takes care of
everything: it roxygenizes the packages if needed, and load them in the
appropriate order:

``` r
plan <- pkg_load('lotr')
```

    ## executing load on aragorn

    ## ℹ Updating aragorn documentation
    ## ℹ Loading aragorn
    ## ℹ Loading aragorn
    ## executing load on bilbo
    ## 
    ## ℹ Updating bilbo documentation
    ## ℹ Loading bilbo
    ## ℹ Loading bilbo
    ## executing load on frodo
    ## 
    ## ℹ Updating frodo documentation
    ## ℹ Loading frodo
    ## ℹ Loading frodo
    ## executing load on galadriel
    ## 
    ## ℹ Updating galadriel documentation
    ## ℹ Loading galadriel
    ## ℹ Loading galadriel
    ## executing load on gandalf
    ## 
    ## ℹ Updating gandalf documentation
    ## ℹ Loading gandalf
    ## ℹ Loading gandalf
    ## executing load on gimli
    ## 
    ## ℹ Updating gimli documentation
    ## ℹ Loading gimli
    ## ℹ Loading gimli
    ## executing load on legolas
    ## 
    ## ℹ Updating legolas documentation
    ## ℹ Loading legolas
    ## ℹ Loading legolas
    ## executing load on elves
    ## 
    ## ℹ Updating elves documentation
    ## ℹ Loading elves
    ## ℹ Loading elves
    ## executing load on hobbits
    ## 
    ## ℹ Updating hobbits documentation
    ## ℹ Loading hobbits
    ## ℹ Loading hobbits
    ## executing load on lotr
    ## 
    ## ℹ Updating lotr documentation
    ## ℹ Loading lotr
    ## ℹ Loading lotr

``` r
print(plan)
```

    ##      package action     params
    ## 1    aragorn   load       TRUE
    ## 2      bilbo   load TRUE, TRUE
    ## 3      frodo   load       TRUE
    ## 4  galadriel   load       TRUE
    ## 5    gandalf   load       TRUE
    ## 6      gimli   load       TRUE
    ## 7    legolas   load       TRUE
    ## 8      elves   load       TRUE
    ## 9    hobbits   load TRUE, TRUE
    ## 10      lotr   load TRUE, TRUE

``` r
print(names(lotr()))
```

    ## [1] "hobbits" "elves"   "dwarves" "humans"  "ainur"

### editing and reloading

Let’s edit the `frodo` package, and change the weapon from `sting` to
`sword`:

``` r
lotr()$hobbits$frodo
```

    ## $first
    ## [1] "Frodo"
    ## 
    ## $last
    ## [1] "Baggins"
    ## 
    ## $weapons
    ## [1] "sting"        "barrow-blade"
    ## 
    ## $race
    ## [1] "hobbit"

``` r
lines <- readLines('frodo/R/main.R')
cat(lines, sep = '\n')
```

    ## #' provides frodo
    ## #' @return as a list
    ## #' @export
    ## frodo <- function() {
    ##   list(
    ##     first = 'Frodo',
    ##     last = 'Baggins',
    ##     weapons = c('sting', 'barrow-blade'),
    ##     race = 'hobbit'
    ##   )
    ## }

``` r
edited_lines <- sub('sting', 'sword', lines)
writeLines(edited_lines, 'frodo/R/main.R')
```

Now let’s ask `srcpkgs` to make sure the `lotr` package is up-to-date:

``` r
plan <- pkg_load('lotr')
```

    ## package /tmp/RtmpwuF77a/file1c6f26a0056f/srcpkgs_lotr_demo/frodo has changed: R/main.R was "modified"

    ## executing unload on lotr

    ## executing unload on hobbits

    ## executing unload on frodo

    ## executing load on frodo

    ## ℹ Updating frodo documentation
    ## ℹ Loading frodo
    ## ℹ Loading frodo
    ## executing load on hobbits
    ## 
    ## ℹ Loading hobbits
    ## executing load on lotr
    ## 
    ## ℹ Loading lotr

``` r
print(plan)
```

    ##   package action params
    ## 1    lotr unload   NULL
    ## 2 hobbits unload   NULL
    ## 3   frodo unload   NULL
    ## 4   frodo   load   TRUE
    ## 5 hobbits   load   TRUE
    ## 6    lotr   load   TRUE

–\> It figured out that `frodo` was modified, and needed to be reloaded,
and for that all its dependents needed to be also properly unloaded and
re-loaded.

``` r
lotr()$hobbits$frodo$weapons
```

    ## [1] "sword"        "barrow-blade"
