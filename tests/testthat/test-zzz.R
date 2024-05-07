# mostly for the test coverage
test_that(".onDetach", {
  old <- is_loaders_hack_installed()
  if (old) on.exit(hack_r_loaders(), add = TRUE)
  
  ###
  hack_r_loaders()

  .onDetach('dummy')

  expect_false(is_loaders_hack_installed())
})