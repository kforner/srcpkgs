PKG=srcpkgs

rox:
	Rscript --no-save -e 'devtools::document()'

check: rox
	Rscript --no-save -e 'devtools::check(".", check_dir = ".checks")'

test: rox
	Rscript --no-save -e 'devtools::test()'

manual: rox
	rm -f $(PKG).pdf
	R CMD Rd2pdf -o $(PKG).pdf .

clean:
	rm -rf .checks* .Rd2*
	