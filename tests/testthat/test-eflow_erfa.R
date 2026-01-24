# Functions under test: R/eflow_erfa.R evaluate_erfa_class()

test_that("evaluate_erfa_class uses per-group sensi_threshold", {
  base <- tibble::tibble(
    metric_key = c("amax1d", "january_flow", "amin1d", "mean_rate_rise", "num_zero_seq"),
    group      = c("HF",     "MF",           "LF",     "RFC",           "IF"),
    stat       = rep("med", 5),
    value      = rep(100, 5)
  )

  scen <- tibble::tibble(
    metric_key = c("amax1d", "january_flow", "amin1d", "mean_rate_rise", "num_zero_seq"),
    group      = c("HF",     "MF",           "LF",     "RFC",           "IF"),
    stat       = rep("med", 5),
    value      = rep(130, 5)  # +30%
  )

  out <- evaluate_erfa_class(
    base, scen,
    sensi_threshold = c(HF = 40, MF = 20, LF = 25, RFC = 35, IF = 30),
    stat = "med",
    metric_set = "erfa70"
  )

  d <- out$detail %>% dplyr::select(metric_key, group, pct_change, sensi_threshold_group, outside_range)

  expect_false(d$outside_range[d$group == "HF"])  # 30 <= 40
  expect_true (d$outside_range[d$group == "MF"])  # 30 >  20
  expect_true (d$outside_range[d$group == "LF"])  # 30 >  25
  expect_false(d$outside_range[d$group == "RFC"]) # 30 <= 35
  expect_false(d$outside_range[d$group == "IF"])  # strict '>' so 30 > 30 is FALSE
})

test_that("evaluate_erfa_class rejects incomplete per-group sensi_threshold", {
  base <- tibble::tibble(
    metric_key = c("amax1d", "january_flow"),
    group      = c("HF",     "MF"),
    stat       = c("med",    "med"),
    value      = c(100,      100)
  )

  scen <- tibble::tibble(
    metric_key = c("amax1d", "january_flow"),
    group      = c("HF",     "MF"),
    stat       = c("med",    "med"),
    value      = c(120,      120)
  )

  expect_error(
    evaluate_erfa_class(
      base, scen,
      sensi_threshold = c(HF = 10, MF = 10),  # missing LF, RFC, IF
      stat = "med",
      metric_set = "erfa70"
    ),
    "missing required group threshold"
  )
})
