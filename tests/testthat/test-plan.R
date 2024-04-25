test_that("execute_plan", {
  setup_temp_dir()
  src_pkgs <- examples_srcpkgs_complex_deps()
  on.exit(cleanup_dangling_srcpkgs(), add = TRUE)
  mat <- graph_from_srcpkgs(src_pkgs)

  mat2 <- sub_graph(mat, loadedNamespaces())

  pkg_unload('EE', src_pkgs, quiet = TRUE) # in case

  ###
  pkg_load('AA', quiet = TRUE)
  mat2 <- sub_graph(mat, loadedNamespaces())
  expect_equal(nrow(mat2), 6)

  uplan <- unload_plan('EE', mat2)
  expect_equal(nrow(uplan), 5)

  execute_plan(uplan, quiet = TRUE)
})

