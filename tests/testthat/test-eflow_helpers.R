# Functions under test: .hydro_as_date(), .hydro_as_numeric(), .hydro_safe_max(),
# .hydro_safe_min(), .hydro_safe_which_max(), .hydro_safe_which_min(),
# .hydro_safe_mean(), .hydro_seq_stats()
# Path: R/eflow_helpers.R

test_that("date and numeric coercion helpers validate inputs", {
  expect_identical(hydrokit:::.hydro_as_date("2001-01-01"), as.Date("2001-01-01"))
  expect_error(hydrokit:::.hydro_as_date(c("2001-01-01", NA_character_)), "contains NA")

  expect_equal(hydrokit:::.hydro_as_numeric(c("1", "2")), c(1, 2), tolerance = 1e-12)
  expect_error(hydrokit:::.hydro_as_numeric(c("x", "y")), "could not be coerced")
})

test_that("safe summary helpers handle NA and finite filtering", {
  x <- c(NA_real_, 2, 5, -1)
  expect_equal(hydrokit:::.hydro_safe_max(x), 5, tolerance = 1e-12)
  expect_equal(hydrokit:::.hydro_safe_min(x), -1, tolerance = 1e-12)
  expect_identical(hydrokit:::.hydro_safe_which_max(c(NA_real_, 2, 5, 1)), 3L)
  expect_identical(hydrokit:::.hydro_safe_which_min(c(NA_real_, 2, 5, -2)), 4L)
  expect_equal(hydrokit:::.hydro_safe_mean(c(NA_real_, 2, 4, Inf)), 3, tolerance = 1e-12)
  expect_true(is.na(hydrokit:::.hydro_safe_mean(c(NA_real_, Inf))))
})

test_that("sequence statistics report run counts and longest-start index", {
  s <- hydrokit:::.hydro_seq_stats(c(FALSE, TRUE, TRUE, FALSE, TRUE))
  expect_identical(s$n_runs, 2L)
  expect_equal(s$mean_len, 1.5, tolerance = 1e-12)
  expect_identical(s$start_idx_longest, 2L)

  s0 <- hydrokit:::.hydro_seq_stats(rep(FALSE, 4))
  expect_identical(s0$n_runs, 0L)
  expect_identical(s0$mean_len, 0)
  expect_true(is.na(s0$start_idx_longest))
})
