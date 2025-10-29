PKG=srcpkgs

rox:
	Rscript --no-save -e 'devtools::document()'

build: rox
	Rscript --no-save -e 'devtools::build()'

check: rox
	_R_CHECK_SYSTEM_CLOCK_=0 Rscript --no-save -e 'devtools::check(".", check_dir = ".checks")'

check/quick/nocran: rox
	_R_CHECK_SYSTEM_CLOCK_=0 Rscript --no-save -e 'devtools::check(".", check_dir = ".checks", vignettes = FALSE, env_vars = NULL)'

check/nocran: rox
	_R_CHECK_SYSTEM_CLOCK_=0 Rscript --no-save -e 'devtools::check(".", check_dir = ".checks", env_vars = NULL)'

check/quick: rox
	_R_CHECK_SYSTEM_CLOCK_=0 Rscript --no-save -e 'devtools::check(".", check_dir = ".checks", vignettes = FALSE)'

run_examples: rox
	Rscript --no-save -e 'devtools::run_examples(run_donttest = TRUE)'

FILTER=
test: rox
	Rscript --no-save -e 'devtools::test(filter="$(FILTER)")'

manual: rox
	rm -f $(PKG).pdf
	R CMD Rd2pdf -o $(PKG).pdf .

clean:
	rm -rf .checks* .Rd2*
	
pkgdown: rox
	Rscript --no-save -e 'pkgdown::build_site()'

zero-coverage:
	Rscript -e 'library(covr); zero_coverage(package_coverage())'

covr:
	NOT_CRAN=true Rscript -e 'library(covr); print(package_coverage())'


COVR_REPORT=.tmp/cov.html
report_covr:
	Rscript -e 'library(covr); print(report(package_coverage(), "$(COVR_REPORT)"))'
