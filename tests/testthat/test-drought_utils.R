# Functions under test: max_dry_spell(), compute_dpi()
# Path: R/drought-metrics.R

test_that("max_dry_spell computes longest run below threshold", {
  p <- c(0, 0.2, 0.5, 3, 0, 0, 0, 5)
  expect_identical(hydrokit::max_dry_spell(p), 3L)
  expect_identical(hydrokit::max_dry_spell(p, threshold = 0.1), 3L)
  expect_identical(hydrokit::max_dry_spell(c(2, 3, 4), threshold = 1), 0L)
})

test_that("compute_dpi is deterministic and ignores NA", {
  x <- c(-1.2, -0.8, NA_real_, -2.0, 0.5)
  expect_equal(hydrokit::compute_dpi(x, threshold = -1), 2 / 4, tolerance = 1e-12)
  expect_identical(hydrokit::compute_dpi(c(NA_real_, NA_real_)), 0)
})
