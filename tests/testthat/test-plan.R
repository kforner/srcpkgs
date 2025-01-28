test_that("execute_plan", {
  src_pkgs <- examples_srcpkgs_complex_deps()

  mat <- graph_from_srcpkgs(src_pkgs)

  pkg_unload('EE', src_pkgs, quiet = TRUE) # in case

  ###
  plan <- pkg_load('AA', src_pkgs, roxygen = FALSE, quiet = TRUE)

  mat2 <- sub_graph(mat, loadedNamespaces())
  expect_equal(nrow(plan), nrow(mat2))

  uplan <- unload_plan('EE', mat2)
  expect_equal(nrow(uplan), 5)

  execute_plan(uplan, src_pkgs, quiet = TRUE)

  ### edge cases
  plan <- data.frame(package = 'AA', action = '')
  expect_error(execute_plan(plan, src_pkgs, quiet = TRUE), 'unknown action')
})

