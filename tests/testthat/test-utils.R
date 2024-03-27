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

test_that("stop_unless", 
{
  stop_unless <- srcpkgs:::stop_unless

  expect_error(stop_unless(TRUE), 'missing')
  expect_error(stop_unless(1:2, 'argh'), 'scalar')

	expect_error(stop_unless(FALSE, 'argh'), 'argh')
  expect_error(stop_unless(length(NULL), 'argh'), 'argh')
  expect_error(stop_unless(length(1), 'argh'), NA)

	# messages
	expect_error(stop_unless(FALSE, "i = %i s = %s",3,'toto'), "i = 3 s = toto")

  # test vectors in additional arguments (...) for sprintf
  expect_error(stop_unless(FALSE, 'ints=%s', 1:3), 'ints=1,2,3')

  # empty condition (logical)
  expect_error(stop_unless(NULL == 1, 'Argh'), 'scalar')

  ### see if the message args are evaluated even if the condition is TRUE
  expect_error(stop_unless(TRUE, stop('beurk')), NA)
})