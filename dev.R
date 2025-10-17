library(devtools)

check_man()

test()
test(filter = "config")
test(filter = "pkg_check")
test(filter = "pkg_load")
test(filter = "pkg_test")
test(filter = "pkgs_check")
test(filter = "pkgs_test")
test(filter = "project_root")
check()

options(width = Sys.getenv("COLUMNS", 80))
