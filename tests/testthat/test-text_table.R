

test_that("print_text_table_with_base", {
  df <- head(iris)
  expect_snapshot(print_text_table_with_base(df))
})

test_that("print_text_table_with_huxtable", {
  # we enable crayon and color output for this test
  local_reproducible_output(crayon = TRUE)
  df <- head(iris)

 withr::local_options(list(cli.num_colors = 256))

  expect_snapshot(print_text_table_with_huxtable(df))
  expect_snapshot(print_text_table_with_huxtable(df, styler = NULL))

  expect_snapshot(print_text_table_with_huxtable(df, title = "Title"))

  expect_snapshot(print_text_table_with_huxtable(df, footnote = "Footnote"))

  expect_snapshot(print_text_table_with_huxtable(df, heatmap_columns = c(1, 3)))

  expect_snapshot(print_text_table_with_huxtable(df, hilite_rows = c(1, 3)))
  
})


