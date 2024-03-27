library(usethis)
create_package('.')
use_gpl3_license()

use_github_action()
use_github_links()
use_testthat()

use_package_doc()
use_pkgdown()
use_pkgdown_github_pages()
use_github_action("pkgdown")

use_coverage()
use_github_action("test-coverage")

