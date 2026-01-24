# ==============================================================================
# Tests for: R/utils.R
# ==============================================================================

test_that(".is_int_scalar validates correctly", {
  expect_true(hydrokit:::.is_int_scalar(1L))
  expect_true(hydrokit:::.is_int_scalar(1))
  expect_false(hydrokit:::.is_int_scalar(1.5))
  expect_false(hydrokit:::.is_int_scalar(NA_integer_))
  expect_false(hydrokit:::.is_int_scalar(c(1L, 2L)))
  expect_false(hydrokit:::.is_int_scalar(NULL))
})

test_that(".is_num_scalar validates correctly", {
  expect_true(hydrokit:::.is_num_scalar(1.5))
  expect_true(hydrokit:::.is_num_scalar(1L))
  expect_false(hydrokit:::.is_num_scalar(NA_real_))
  expect_false(hydrokit:::.is_num_scalar(c(1, 2)))
  expect_false(hydrokit:::.is_num_scalar("1"))
})

test_that(".validate_numeric_vector catches invalid input", {
  expect_silent(hydrokit:::.validate_numeric_vector(c(1, 2, 3)))
  expect_silent(hydrokit:::.validate_numeric_vector(c(1, NA, 3), allow_na = TRUE))
  expect_error(hydrokit:::.validate_numeric_vector("a"), "must be numeric")
  expect_error(hydrokit:::.validate_numeric_vector(c(1, NA), allow_na = FALSE), "contains NA")
})
