# Functions under test: calculate_hydro_metrics(), .hydro_metric_registry_base(),
# .hydro_annual_context(), .hydro_build_threshold_object(), .hydro_validate_registry()
# Paths: R/hydro_metrics.R

testthat::test_that("registry compute matches annual output for a single year", {
  old_tz <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "UTC")
  on.exit({
    if (is.na(old_tz)) {
      Sys.unsetenv("TZ")
    } else {
      Sys.setenv(TZ = old_tz)
    }
  }, add = TRUE)

  set.seed(123)
  datev <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
  Qy <- 10 + sin(seq_along(datev) / 20) + stats::rnorm(length(datev), sd = 0.5)
  Qy[seq(5, length(Qy), by = 17)] <- NA_real_

  jdy <- lubridate::yday(datev)
  mony <- lubridate::month(datev)

  thr_obj <- hydrokit:::.hydro_build_threshold_object(
    Qv = Qy,
    datev = datev,
    threshold_scope = "global",
    high_q = 0.9,
    low_q = 0.1,
    zero_thr_mode = "absolute",
    zero_thr = 0.02,
    zero_q = 0.02,
    zero_frac_median = 0.001,
    min_finite_thresholds = 10L,
    baseline_years = NULL,
    baseline_dates = NULL
  )

  thr_high_vec <- rep(thr_obj$high, length(Qy))
  thr_low_vec <- rep(thr_obj$low, length(Qy))
  thr_zero_vec <- rep(thr_obj$zero, length(Qy))

  ctx <- hydrokit:::.hydro_annual_context(
    Qy = Qy, jdy = jdy, mony = mony, y = 2001,
    thr_high_vec = thr_high_vec,
    thr_low_vec = thr_low_vec,
    thr_zero_vec = thr_zero_vec,
    roc_eps = 0,
    roc_smooth_k = 1L,
    month_min_frac = 0,
    report_coverage = FALSE
  )

  reg <- hydrokit:::.hydro_metric_registry_base()
  keys <- hydrokit:::.hydro_erfa70_base_order()
  reg_use <- reg[match(keys, reg$metric_key), , drop = FALSE]

  vals <- numeric(length(keys))
  for (k in seq_along(keys)) {
    vals[k] <- reg_use$compute[[k]](ctx)
  }
  names(vals) <- keys

  res <- hydrokit:::calculate_hydro_metrics(
    Q = Qy,
    date = datev,
    metrics = keys,
    summaries = c("med", "iqr"),
    return_annual = TRUE
  )

  annual_tbl <- res$annual
  for (k in keys) {
    val <- annual_tbl$value[annual_tbl$metric_key == k]
    testthat::expect_equal(val, vals[[k]], tolerance = 1e-12)
  }
})

testthat::test_that("summary outputs for ERFA base metrics match registry annual compute", {
  old_tz <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "UTC")
  on.exit({
    if (is.na(old_tz)) {
      Sys.unsetenv("TZ")
    } else {
      Sys.setenv(TZ = old_tz)
    }
  }, add = TRUE)

  set.seed(456)
  datev <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day")
  Q <- 7 + sin(seq_along(datev) / 30) + stats::rnorm(length(datev), sd = 0.4)

  keys <- hydrokit:::.hydro_erfa70_base_order()

  thr_obj <- hydrokit:::.hydro_build_threshold_object(
    Qv = Q,
    datev = datev,
    threshold_scope = "global",
    high_q = 0.9,
    low_q = 0.1,
    zero_thr_mode = "absolute",
    zero_thr = 0.02,
    zero_q = 0.02,
    zero_frac_median = 0.001,
    min_finite_thresholds = 10L,
    baseline_years = NULL,
    baseline_dates = NULL
  )

  reg <- hydrokit:::.hydro_metric_registry_base()
  reg_use <- reg[match(keys, reg$metric_key), , drop = FALSE]

  yrs <- sort(unique(lubridate::year(datev)))
  annual_vals <- matrix(NA_real_, nrow = length(yrs), ncol = length(keys))
  colnames(annual_vals) <- keys

  for (i in seq_along(yrs)) {
    y <- yrs[i]
    idx <- which(lubridate::year(datev) == y)
    Qy <- Q[idx]
    jdy <- lubridate::yday(datev[idx])
    mony <- lubridate::month(datev[idx])

    thr_high_vec <- rep(thr_obj$high, length(Qy))
    thr_low_vec <- rep(thr_obj$low, length(Qy))
    thr_zero_vec <- rep(thr_obj$zero, length(Qy))

    ctx <- hydrokit:::.hydro_annual_context(
      Qy = Qy, jdy = jdy, mony = mony, y = y,
      thr_high_vec = thr_high_vec,
      thr_low_vec = thr_low_vec,
      thr_zero_vec = thr_zero_vec,
      roc_eps = 0,
      roc_smooth_k = 1L,
      month_min_frac = 0,
      report_coverage = FALSE
    )

    for (k in seq_along(keys)) {
      annual_vals[i, k] <- reg_use$compute[[k]](ctx)
    }
  }

  expected <- list()
  for (k in seq_along(keys)) {
    mk <- keys[k]
    expected[[mk]] <- c(
      med = stats::median(annual_vals[, k], na.rm = TRUE),
      iqr = stats::IQR(annual_vals[, k], na.rm = TRUE)
    )
  }

  out <- hydrokit:::calculate_hydro_metrics(
    Q = Q,
    date = datev,
    metrics = keys,
    summaries = c("med", "iqr")
  )

  for (k in seq_along(keys)) {
    mk <- keys[k]
    for (st in c("med", "iqr")) {
      val <- out$value[out$metric_key == mk & out$stat == st]
      testthat::expect_equal(val, expected[[mk]][[st]], tolerance = 1e-12)
    }
  }
})

testthat::test_that("ERFA output structure, metric_id mapping, and ordering are stable", {
  set.seed(789)
  datev <- seq(as.Date("2004-01-01"), as.Date("2005-12-31"), by = "day")
  Q <- 9 + sin(seq_along(datev) / 22) + stats::rnorm(length(datev), sd = 0.3)

  out <- hydrokit:::calculate_hydro_metrics(
    Q = Q,
    date = datev,
    metric_set = "erfa70",
    summaries = c("med", "iqr")
  )

  expected_cols <- c("metric_id", "metric_key", "group", "stat", "metric", "value")
  testthat::expect_identical(names(out), expected_cols)

  keys <- hydrokit:::.hydro_erfa70_base_order()
  expected_keys <- rep(keys, times = 2L)
  expected_stats <- rep(c("med", "iqr"), each = length(keys))
  testthat::expect_identical(out$metric_key, expected_keys)
  testthat::expect_identical(out$stat, expected_stats)

  erfa70 <- hydrokit:::.hydro_registry_erfa70_expanded()
  expected_id <- erfa70$metric_id[match(
    paste(out$metric_key, out$stat, out$metric, out$group),
    paste(erfa70$metric_key, erfa70$stat, erfa70$metric, erfa70$group)
  )]
  testthat::expect_identical(out$metric_id, expected_id)
})

testthat::test_that("registry validation passes in strict mode", {
  testthat::expect_silent(hydrokit:::.hydro_validate_registry(strict = TRUE))
})
