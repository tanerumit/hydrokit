# Functions under test: evaluate_erfa_class()
# Path: R/eflow_erfa.R

test_that("evaluate_erfa_class uses per-group sensi_threshold", {
  metric_def <- hydrokit:::.hydro_registry_erfa70_expanded()
  metric_def <- metric_def[metric_def$stat == "med", , drop = FALSE]
  keep_idx <- c(
    which(metric_def$group == "HF")[1],
    which(metric_def$group == "MF")[1],
    which(metric_def$group == "LF")[1],
    which(metric_def$group == "RFC")[1],
    which(metric_def$group == "IF")[1]
  )
  metric_def <- metric_def[keep_idx, , drop = FALSE]

  base <- tibble::tibble(
    metric_id = metric_def$metric_id,
    value = rep(100, nrow(metric_def))
  )

  scen <- tibble::tibble(
    metric_id = metric_def$metric_id,
    value = rep(130, nrow(metric_def))
  )

  out <- hydrokit::evaluate_erfa_class(
    baseline_tbl = base,
    scenario_tbl = scen,
    sensi_threshold = c(HF = 40, MF = 20, LF = 25, RFC = 35, IF = 30),
    metric_set = "erfa70"
  )

  d <- out$detail[, c("group", "pct_change", "sensi_threshold_group", "outside_range")]

  expect_false(d$outside_range[d$group == "HF"])
  expect_true(d$outside_range[d$group == "MF"])
  expect_true(d$outside_range[d$group == "LF"])
  expect_false(d$outside_range[d$group == "RFC"])
  expect_false(d$outside_range[d$group == "IF"])
})

test_that("evaluate_erfa_class rejects incomplete per-group sensi_threshold", {
  metric_def <- hydrokit:::.hydro_registry_erfa70_expanded()
  metric_def <- metric_def[metric_def$stat == "med", , drop = FALSE]
  metric_def <- metric_def[seq_len(2), , drop = FALSE]

  base <- tibble::tibble(
    metric_id = metric_def$metric_id,
    value = c(100, 100)
  )
  scen <- tibble::tibble(
    metric_id = metric_def$metric_id,
    value = c(120, 120)
  )

  expect_error(
    hydrokit::evaluate_erfa_class(
      baseline_tbl = base,
      scenario_tbl = scen,
      sensi_threshold = c(HF = 10, MF = 10),
      metric_set = "erfa70"
    ),
    "missing required group threshold"
  )
})

test_that("evaluate_erfa_class applies group_cols and scenario_cols semantics", {
  metric_ids <- hydrokit:::.hydro_registry_erfa70_expanded()$metric_id[1:4]

  base <- tibble::tibble(
    site = c("A", "A", "B", "B"),
    metric_id = metric_ids,
    value = c(100, 100, 100, 100)
  )

  scen <- tibble::tibble(
    site = c("A", "A", "B", "B", "C", "C"),
    scenario = c("dry", "dry", "dry", "dry", "dry", "dry"),
    metric_id = c(metric_ids, metric_ids[1:2]),
    value = c(120, 120, 80, 80, 150, 150)
  )

  out <- hydrokit::evaluate_erfa_class(
    baseline_tbl = base,
    scenario_tbl = scen,
    sensi_threshold = 10,
    metric_set = "erfa70",
    group_cols = "site",
    scenario_cols = "scenario"
  )

  expect_false(any(out$detail$site == "C"))
  expect_true(all(c("site", "scenario") %in% names(out$summary)))
  expect_true(any(out$summary$group == "Overall"))
})
