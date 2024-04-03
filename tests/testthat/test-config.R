test_that("config", {
  KEY <- 'toto'

  on.exit(delete_config(KEY), add = TRUE)
  delete_config(KEY)

  ### get_config
  # no config set
  expect_null(get_config(KEY))

  # set a value
  expect_null(set_config(KEY, 'value'))
  expect_identical(get_config(KEY), 'value')

  # set another value
  value <- head(iris, 5)
  expect_identical(set_config(KEY, value), 'value')
  expect_identical(get_config(KEY), value)
  
  # delete: with value
  expect_identical(delete_config(KEY), value)
  expect_null(get_config(KEY))
  
  # delete: no value
  expect_null(delete_config(KEY))
  expect_null(get_config(KEY))
})
