# ==============================================================================
# Script: R/eflow_erfa.R
# Functions under test: evaluate_erfa_class()
# Purpose: Compute ERFA class summaries from registry-selected metric comparisons.
# ==============================================================================

#' Calculate Environmental Flow Risk Assessment (ERFA) Class
#'
#' @description
#' Compares a registry-defined subset of hydrological metrics (default: ERFA70)
#' between baseline and scenario summaries, counts metrics outside a sensitivity
#' threshold by group, and assigns ERFA risk classes.
#'
#' @details
#' Inputs should be summary tables returned by \code{calculate_hydro_metrics()} and
#' must contain \code{metric_key}, \code{group}, \code{stat}, and \code{value}. If a
#' \code{site} column is present in either table, comparisons are performed per site.
#' Percent change is computed as \code{100 * (scenario - baseline) / baseline}; when
#' the baseline value is non-finite or zero, percent change is set to NA and the
#' metric is not counted as outside the threshold. Only metrics present in both
#' tables are compared (inner join).
#'
#' @param baseline_tbl Summary table from \code{calculate_hydro_metrics()} representing
#'   baseline conditions.
#' @param scenario_tbl Summary table from \code{calculate_hydro_metrics()} representing
#'   scenario conditions.
#' @param sensi_threshold Numeric scalar > 0. Absolute percent-change threshold used
#'   to flag metrics as outside the sensitivity range (default 30).
#' @param stat Character scalar. Summary statistic to compare (default "med").
#' @param metric_set Character scalar. Registered metric set to evaluate (default
#'   "erfa70").
#'
#' @return
#' A list with:
#' \itemize{
#' \item \code{detail}: metric-level comparison table with baseline/scenario values,
#'   percent change, and a logical \code{outside_range} flag.
#' \item \code{summary}: group-level and overall ERFA classification table with counts,
#'   percent outside, and risk class/label.
#' }
#'
#' @examples
#' date <- seq.Date(as.Date("2001-01-01"), as.Date("2002-12-31"), by = "day")
#' Q <- 2 + sin(seq_along(date) * 2 * pi / 365)
#' baseline <- calculate_hydro_metrics(Q, date)
#' scenario <- calculate_hydro_metrics(Q * 1.1, date)
#' erfa <- evaluate_erfa_class(baseline, scenario, sensi_threshold = 30, stat = "med")
#' erfa$summary
#' @export
evaluate_erfa_class <- function(
    baseline_tbl,
    scenario_tbl,
    sensi_threshold = 30,
    stat = "med",
    metric_set = "erfa70"
) {

  # Validate required ERFA columns; args: x table-like object, name character scalar.
  .hydro_erfa_validate_tbl <- function(x, name) {
    req <- c("metric_key", "group", "stat", "value")
    miss <- setdiff(req, names(x))
    if (length(miss) > 0L) {
      stop(name, " is missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
    }
    invisible(TRUE)
  }

  # Determine join keys for baseline/scenario tables; args: baseline_tbl, scenario_tbl data frames.
  .hydro_erfa_join_keys <- function(baseline_tbl, scenario_tbl) {
    if ("site" %in% names(baseline_tbl) || "site" %in% names(scenario_tbl)) {
      return(c("site", "metric_key", "stat"))
    }
    c("metric_key", "stat")
  }

  .hydro_erfa_validate_tbl(baseline_tbl, "baseline_tbl")
  .hydro_erfa_validate_tbl(scenario_tbl, "scenario_tbl")

  required_keys <- .hydro_resolve_metric_keys(metric_set = metric_set, metrics = NULL, purpose = NULL)

  base_f <- baseline_tbl[
    baseline_tbl$metric_key %in% required_keys & baseline_tbl$stat == stat,
    ,
    drop = FALSE
  ]

  scen_f <- scenario_tbl[
    scenario_tbl$metric_key %in% required_keys & scenario_tbl$stat == stat,
    ,
    drop = FALSE
  ]

  join_by <- .hydro_erfa_join_keys(base_f, scen_f)

  df <- base_f |>
    dplyr::select(dplyr::all_of(intersect(c("site", "metric_id", "metric_key", "group", "stat", "metric", "value"), names(base_f)))) |>
    dplyr::rename(value_base = value) |>
    dplyr::inner_join(
      scen_f |>
        dplyr::select(dplyr::all_of(intersect(c("site", "metric_key", "stat", "value"), names(scen_f)))) |>
        dplyr::rename(value_scen = value),
      by = join_by
    ) |>
    dplyr::mutate(
      pct_change = dplyr::if_else(
        is.finite(value_base) & value_base != 0,
        100 * (value_scen - value_base) / value_base,
        NA_real_
      ),
      outside_range = abs(pct_change) > sensi_threshold
    )

  summary_tbl <- df |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_outside = sum(outside_range, na.rm = TRUE),
      perc_outside = 100 * n_outside / n_total,
      .groups = "drop"
    )

  total_row <- summary_tbl |>
    dplyr::summarise(
      group = "Overall",
      n_total = sum(n_total),
      n_outside = sum(n_outside),
      perc_outside = 100 * n_outside / n_total
    )

  summary_tbl <- dplyr::bind_rows(summary_tbl, total_row)

  risk_thresholds <- tibble::tribble(
    ~group, ~low, ~medium, ~high,
    "Overall", 1, 23, 47,
    "HF", 1, 6, 11,
    "MF", 1, 9, 17,
    "LF", 1, 6, 11,
    "RFC", 1, 3, 6,
    "IF", 1, 3, 5
  )

  summary_tbl <- summary_tbl |>
    dplyr::left_join(risk_thresholds, by = "group") |>
    dplyr::mutate(
      risk_class = dplyr::case_when(
        n_outside == 0 ~ 0,
        n_outside >= high ~ 3,
        n_outside >= medium ~ 2,
        n_outside >= low ~ 1,
        TRUE ~ NA_real_
      ),
      risk_label = factor(
        risk_class,
        levels = 0:3,
        labels = c(
          "No change (\U0001F7E6 Blue)",
          "Low (\U0001F7E9 Green)",
          "Medium (\U0001F7E7 Amber)",
          "High (\U0001F7E5 Red)"
        )
      )
    ) |>
    dplyr::select(group, n_total, n_outside, perc_outside, risk_class, risk_label) |>
    dplyr::mutate(
      group = factor(group, levels = c("HF", "MF", "LF", "RFC", "IF", "Overall"), ordered = TRUE)
    ) |>
    dplyr::arrange(group)
  list(detail = df, summary = summary_tbl)
}

