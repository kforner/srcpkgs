PKG=srcpkgs

rox:
	Rscript --no-save -e 'devtools::document()'

check: rox
	Rscript --no-save -e 'devtools::check(".", check_dir = ".checks")'

FILTER=
test: rox
	Rscript --no-save -e 'devtools::test(filter="$(FILTER)")'

manual: rox
	rm -f $(PKG).pdf
	R CMD Rd2pdf -o $(PKG).pdf .

clean:
	rm -rf .checks* .Rd2*
	
pkgdown:
	Rscript --no-save -e 'pkgdown::build_site()'