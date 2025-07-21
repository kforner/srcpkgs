library(devtools)


check_man()

test()
test(filter = "pkg_load")
test(filter = "pkg_check")
check()
