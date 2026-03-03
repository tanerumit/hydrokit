# Functions under test: list_metrics(), calculate_flow_metrics(),
# summarize_flow_metrics(), .metric_catalog_flow(), .flow_metric_registry(),
# .metric_set_erfa70(), .resolve_metric_set()
# Paths: R/catalog.R, R/flow-catalog.R, R/flow-registry.R, R/flow-calculate.R,
# R/flow-summarize.R, R/erfa-metric-set.R

testthat::test_that("list_metrics returns expected columns and supports family filter", {
  all_metrics <- hydrokit::list_metrics()
  expected_cols <- c("key", "family", "group", "grain", "label", "input")
  testthat::expect_identical(names(all_metrics), expected_cols)
  testthat::expect_true(all(c("flow", "water_resources", "drought") %in% unique(all_metrics$family)))
  testthat::expect_identical(anyDuplicated(all_metrics$key), 0L)

  flow_metrics <- hydrokit::list_metrics(family = "flow")
  testthat::expect_true(nrow(flow_metrics) > 0L)
  testthat::expect_true(all(flow_metrics$family == "flow"))
})

testthat::test_that("flow registry metadata is constructed from flow catalog", {
  catalog <- hydrokit:::.metric_catalog_flow()
  registry <- hydrokit:::.flow_metric_registry()

  testthat::expect_identical(nrow(registry), nrow(catalog))
  testthat::expect_identical(registry$metric_key, catalog$key)
  testthat::expect_identical(registry$group, catalog$group)
  testthat::expect_identical(registry$grain, catalog$grain)
  testthat::expect_identical(registry$label, catalog$label)
  testthat::expect_silent(hydrokit:::.flow_validate_registry(strict = TRUE))
})

testthat::test_that("ERFA metric-set descriptor resolves metadata via flow catalog", {
  set_desc <- hydrokit:::.metric_set_erfa70()
  flow_catalog <- hydrokit:::.metric_catalog_flow()
  idx <- match(set_desc$keys, flow_catalog$key)

  testthat::expect_false(anyNA(idx))
  testthat::expect_identical(set_desc$metadata$key, set_desc$keys)
  testthat::expect_identical(set_desc$metadata$group, flow_catalog$group[idx])
  testthat::expect_identical(set_desc$metadata$grain, flow_catalog$grain[idx])
  testthat::expect_identical(set_desc$metadata$label, flow_catalog$label[idx])

  resolved <- hydrokit:::.resolve_metric_set("erfa70")
  testthat::expect_identical(resolved$keys, set_desc$keys)
  testthat::expect_identical(resolved$metadata$key, set_desc$metadata$key)
})

testthat::test_that("flow calculate/summarize aliases keep hydro outputs stable", {
  set.seed(42)
  datev <- seq(as.Date("2001-01-01"), as.Date("2004-12-31"), by = "day")
  Q <- 8 + sin(seq_along(datev) / 30) + stats::rnorm(length(datev), sd = 0.3)

  annual_h <- hydrokit::calculate_hydro_metrics(Q = Q, date = datev, metrics = "erfa70")
  annual_f <- hydrokit::calculate_flow_metrics(Q = Q, date = datev, metrics = "erfa70")
  testthat::expect_equal(annual_f, annual_h, tolerance = 1e-12)

  summary_h <- hydrokit::summarize_hydro_metrics(annual_h, summaries = c("med", "iqr"))
  summary_f <- hydrokit::summarize_flow_metrics(annual_f, summaries = c("med", "iqr"))
  testthat::expect_equal(summary_f, summary_h, tolerance = 1e-12)
  testthat::expect_true(all(c("metric_id", "metric_key", "group", "stat", "metric", "value") %in% names(summary_f)))
})
