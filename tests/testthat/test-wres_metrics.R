# Functions under test: reliability_hashimoto(), resilience_hashimoto(),
# vulnerability_hashimoto(), failure_frequency(), mean_failure_duration(),
# mean_recovery_time(), vulnerability_event_hashimoto(),
# extract_state_hashimoto(), failure_metrics_hashimoto()
# Path: R/wr-metrics.R

test_that("Hashimoto core metrics return expected values on simple series", {
  obs <- rep(10, 6)
  sim <- c(12, 8, 7, 11, 9, 10)

  expect_equal(hydrokit::reliability_hashimoto(obs, sim), 0.5, tolerance = 1e-12)
  expect_equal(hydrokit::resilience_hashimoto(obs, sim), 2 / 3, tolerance = 1e-12)
  expect_equal(hydrokit::vulnerability_hashimoto(obs, sim, statistic = "mean"), 2, tolerance = 1e-12)
  expect_equal(hydrokit::vulnerability_hashimoto(obs, sim, statistic = "max"), 3, tolerance = 1e-12)
  expect_equal(hydrokit::failure_frequency(obs, sim), 2 / 6, tolerance = 1e-12)
  expect_equal(hydrokit::mean_failure_duration(obs, sim), 1.5, tolerance = 1e-12)
  expect_equal(hydrokit::mean_recovery_time(obs, sim), 1.5, tolerance = 1e-12)
})

test_that("NA handling is deterministic for omit and fail modes", {
  obs <- c(10, 10, 10, NA_real_)
  sim <- c(12, 8, NA_real_, 10)

  expect_equal(hydrokit::reliability_hashimoto(obs, sim, na_action = "omit"), 0.5, tolerance = 1e-12)
  expect_error(hydrokit::reliability_hashimoto(obs, sim, na_action = "fail"), "Non-finite values")
})

test_that("extract_state_hashimoto returns events and transitions consistently", {
  obs <- rep(10, 6)
  sim <- c(12, 8, 7, 11, 9, 10)

  st <- hydrokit::extract_state_hashimoto(obs, sim)

  expect_identical(st$n_steps, 6L)
  expect_identical(st$n_fail_events, 2L)
  expect_identical(st$fail_event_lengths, c(2L, 1L))
  expect_identical(st$fail_event_starts, c(2L, 5L))
  expect_identical(st$fail_event_ends, c(3L, 5L))
  expect_identical(st$n_transitions, 5L)
  expect_identical(length(st$is_fail_t), 5L)
  expect_identical(length(st$is_success_tp1), 5L)
})

test_that("vulnerability_event_hashimoto returns expected event summaries", {
  obs <- rep(10, 8)
  sim <- c(12, 8, 7, 11, 9, 8, 10, 12)

  out <- hydrokit::vulnerability_event_hashimoto(
    obs_threshold = obs,
    sim_supply = sim,
    event_stat = "sum",
    event_agg = "mean",
    return_components = TRUE
  )

  expect_equal(out$vulnerability, 4, tolerance = 1e-12)
  expect_identical(out$n_events, 2L)
  expect_equal(out$event_values, c(5, 3), tolerance = 1e-12)
  expect_identical(out$event_lengths, c(2L, 2L))
})

test_that("failure_metrics_hashimoto handles no-failure paths", {
  obs <- rep(10, 4)
  sim <- rep(11, 4)

  out <- hydrokit::failure_metrics_hashimoto(obs, sim, no_failure = "zero")
  expect_identical(out$n_fail_events, 0L)
  expect_identical(out$n_fail_steps, 0L)
  expect_equal(out$fail_frequency, 0, tolerance = 1e-12)
  expect_equal(out$mean_fail_duration, 0, tolerance = 1e-12)
  expect_equal(out$mean_event_peak_deficit, 0, tolerance = 1e-12)
})
