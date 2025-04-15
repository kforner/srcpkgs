.subset_s3_list <- 
test_that(".subset_s3_list", {
  ### regular list
  lst <- as.list(1:5)
  expect_identical(subset_s3_list(lst, 2), lst[2])
  
  ###
  class(lst) <- "toto"
  res <- subset_s3_list(lst, 2)
  expect_identical(unclass(res), lst[2])
  expect_identical(class(res), "toto")
  
  res <- subset_s3_list(lst, 2:3)
  expect_identical(unclass(res), lst[2:3])
  expect_identical(class(res), "toto")
})



test_that("get_text_logger", {
  expect_identical(get_text_logger(), cli::cli_text)
  logger <- get_text_logger(quiet = TRUE)
  expect_identical(as.character(body(logger)), '{')
})

test_that("is_condition_true", {
  is_condition_true <- srcpkgs:::is_condition_true

  expect_true(is_condition_true(TRUE))
  expect_true(is_condition_true(length("A")))
  expect_true(is_condition_true(1L))
  
  expect_false(is_condition_true(FALSE))
  expect_false(is_condition_true(length(NULL)))
  expect_false(is_condition_true(0L))

  expect_error(is_condition_true(NA), 'missing')
  expect_error(is_condition_true(1:2), 'scalar')
  expect_error(is_condition_true(NULL), 'scalar')
})


test_that("enhanced_sprintf", 
{
  enhanced_sprintf <- srcpkgs:::enhanced_sprintf

  ### edge cases
  expect_identical(enhanced_sprintf(NA), NA)
  expect_identical(enhanced_sprintf(''), '')
  expect_identical(enhanced_sprintf('toto'), 'toto')
  expect_error(enhanced_sprintf(NULL), 'must be a scalar')
  expect_error(enhanced_sprintf(c('toto', 'titi')), 'must be a scalar')

  # empty sprintf input
  expect_identical(enhanced_sprintf('A%sB', NULL), 'AB')

  # normal behaviour
  expect_identical(enhanced_sprintf('toto%i %s %i', 1, 'titi', 5), 'toto1 titi 5')

  ### vectorized input
  # ints => must use %s
  expect_error(enhanced_sprintf('toto%i', 1:3), 'invalid format')
  expect_identical(enhanced_sprintf('toto %s', 1:3), 'toto 1,2,3')

  ### vectorized strings
  expect_identical(enhanced_sprintf('toto %s', LETTERS[1:3]), 'toto A,B,C')
  expect_identical(enhanced_sprintf("i = %i s = %s",3,'toto'), 'i = 3 s = toto')
})


test_that("safe_enhanced_sprintf", 
{
  safe_enhanced_sprintf <- srcpkgs:::safe_enhanced_sprintf


  # normal behaviour
  expect_identical(safe_enhanced_sprintf('toto%i %s %i', 1, 'titi', 5), 'toto1 titi 5')
  expect_identical(safe_enhanced_sprintf('toto %s', 1:3), 'toto 1,2,3')

  # safe behaviour
  expect_match(mute(safe_enhanced_sprintf("", stop('error'))), "could not build error message")

})

test_that("stop_unless", 
{
  stop_unless <- srcpkgs:::stop_unless

  ### no stop
  expect_error(stop_unless(TRUE, 'argh'), NA)
  expect_error(stop_unless(length(1), 'argh'), NA)
  # test if the message args are evaluated even if the condition is TRUE
  expect_error(stop_unless(TRUE, stop('beurk')), NA)

  ### expected stop
	expect_error(stop_unless(FALSE, 'argh'), 'argh')
  expect_error(stop_unless(length(NULL), 'argh'), 'argh')
  # sprintf message
	expect_error(stop_unless(FALSE, "i = %i s = %s",3,'toto'), "i = 3 s = toto")
  expect_error(stop_unless(FALSE, 'ints=%s', 1:3), 'ints=1,2,3')

  ### errors
  # error in the call --> bad params
  expect_error(stop_unless(TRUE), 'format')

  # error in the condition
  expect_error(stop_unless(1:2, 'argh'), 'scalar')
  expect_error(stop_unless(NA, 'argh'), 'missing')
  expect_error(stop_unless(NULL == 1, 'Argh'), 'scalar')
})

test_that("stop_if", 
{
  stop_if <- srcpkgs:::stop_if

  ### no stop
  expect_error(stop_if(FALSE, 'argh'), NA)
  expect_error(stop_if(length(NULL), 'argh'), NA)
  # test if the message args are evaluated even if the condition is TRUE
  expect_error(stop_if(FALSE, stop('beurk')), NA)

  ### expected stop
	expect_error(stop_if(TRUE, 'argh'), 'argh')
  expect_error(stop_if(length(1), 'argh'), 'argh')
  # sprintf message
	expect_error(stop_if(TRUE, "i = %i s = %s",3,'toto'), "i = 3 s = toto")
  expect_error(stop_if(TRUE, 'ints=%s', 1:3), 'ints=1,2,3')

  ### errors
  # error in the call --> bad params
  expect_error(stop_if(FALSE), 'format')

  # error in the condition
  expect_error(stop_if(1:2, 'argh'), 'scalar')
  expect_error(stop_if(NA, 'argh'), 'missing')
  expect_error(stop_if(NULL == 1, 'Argh'), 'scalar')
})

test_that("reproducible_sort", {
  expect_identical(reproducible_sort(rev(LETTERS)), LETTERS)
})


test_that("file_size", {
  setup_temp_dir()
  path <- 'toto'

  # empty file
  writeLines(character(0), path)
  expect_equal(file_size(path), 0)
  
  # at least one char
  writeLines('', path)
  expect_gt(file_size(path), 0)
})
