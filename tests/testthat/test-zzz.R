# mostly for the test coverage
test_that(".onDetach", {
  on.exit(unhack_r_loaders(), add = TRUE)
  
  ###
  hack_r_loaders()

  .onDetach('dummy')

  expect_false(is_loaders_hack_installed())
})
