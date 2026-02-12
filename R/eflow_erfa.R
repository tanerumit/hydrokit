#' Calculate Environmental Flow Risk Assessment (ERFA) Class
#'
#' @description
#' Compare a registry-defined ERFA metric set between baseline and scenario summaries,
#' count threshold exceedances by ERFA group, and assign ERFA risk classes.
#'
#' @details
#' This function compares baseline vs scenario **values indexed by `metric_id`**.
#' All ERFA metadata (group assignment, base `metric_key`, summary statistic `stat`,
#' and legacy metric name `metric`) are taken from the metric-set registry.
#'
#' If `group_cols` is provided, ERFA is computed separately for each unique
#' combination of grouping column values found in the inputs. Technically this is
#' implemented by joining baseline and scenario on `metric_id` plus `group_cols`,
#' then summarising within each group-combination.
#'
#' @param baseline_tbl `data.frame`/tibble with at minimum:
#'   - `metric_id` (integer-like)
#'   - `value` (numeric)
#'   If `group_cols` is not `NULL`, those columns must also be present.
#'
#' @param scenario_tbl Same requirements as `baseline_tbl`.
#'
#' @param sensi_threshold Numeric sensitivity threshold(s) in **percent** for exceedance counting.
#'   Either a single numeric scalar (recycled to all ERFA groups), or a named numeric vector
#'   with names `HF`, `MF`, `LF`, `RFC`, and `IF`. Values must be finite and `>= 0`.
#'
#' @param metric_set Character scalar metric-set name. Currently supports `"erfa70"`
#'   which resolves the full expanded mapping via `.hydro_registry_erfa70_expanded()`.
#'
#' @param group_cols Character vector of grouping columns to evaluate ERFA per level/combination.
#'   Default `NULL` computes a single ERFA result for the entire tables. Examples:
#'   - `group_cols = "site"`: compute per site
#'   - `group_cols = c("site", "scenario")`: compute per site×scenario combination
#'
#' @return A list with:
#'   - `detail`: per-metric comparisons including registry metadata and computed fields.
#'   - `summary`: per-group and overall counts and ERFA risk class/label, per grouping combination.
#'
#' @import dplyr
#' @export
evaluate_erfa_class <- function(
    baseline_tbl,
    scenario_tbl,
    sensi_threshold = 30,
    metric_set = "erfa70",
    group_cols = NULL,
    scenario_cols = NULL
) {

  # ---- helpers ----------------------------------------------------------------

  .hydro_erfa_validate_cols <- function(x, name, req) {
    miss <- setdiff(req, names(x))
    if (length(miss) > 0L) {
      stop(name, " is missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
    }
    invisible(TRUE)
  }

  .hydro_erfa_normalize_cols <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.character(x)) stop("Grouping column arguments must be NULL or a character vector.", call. = FALSE)
    x <- unique(stats::na.omit(trimws(x)))
    if (length(x) == 0L) return(NULL)
    if (any(!nzchar(x))) stop("Grouping column arguments contain empty column name(s).", call. = FALSE)
    x
  }

  .hydro_erfa_resolve_metric_def <- function(metric_set) {
    metric_set <- as.character(metric_set)[1]
    if (identical(metric_set, "erfa70")) return(.hydro_registry_erfa70_expanded())
    stop("Unknown metric_set for ERFA evaluation: ", metric_set, call. = FALSE)
  }

  .hydro_erfa_resolve_sensi_thresholds <- function(sensi_threshold) {
    groups <- c("HF", "MF", "LF", "RFC", "IF")

    if (!is.numeric(sensi_threshold) || length(sensi_threshold) < 1L) {
      stop("sensi_threshold must be a numeric scalar or a named numeric vector.", call. = FALSE)
    }
    if (anyNA(sensi_threshold) || any(!is.finite(sensi_threshold))) {
      stop("sensi_threshold must not contain NA/Inf values.", call. = FALSE)
    }
    if (any(sensi_threshold < 0)) {
      stop("sensi_threshold values must be >= 0.", call. = FALSE)
    }

    if (length(sensi_threshold) == 1L) {
      return(stats::setNames(rep(as.numeric(sensi_threshold), length(groups)), groups))
    }

    nm <- names(sensi_threshold)
    if (is.null(nm) || any(!nzchar(nm))) {
      stop(
        "When providing multiple sensi_threshold values, supply a named vector with names including: ",
        paste(groups, collapse = ", "), ".",
        call. = FALSE
      )
    }

    nm <- trimws(nm)
    bad <- setdiff(nm, c(groups, "Overall"))
    if (length(bad) > 0L) {
      stop(
        "Unknown sensi_threshold group name(s): ", paste(bad, collapse = ", "),
        ". Allowed: ", paste(c(groups, "Overall"), collapse = ", "), ".",
        call. = FALSE
      )
    }

    missing <- setdiff(groups, nm)
    if (length(missing) > 0L) {
      stop(
        "sensi_threshold is missing required group threshold(s): ",
        paste(missing, collapse = ", "), ".",
        call. = FALSE
      )
    }

    out <- sensi_threshold[groups]
    stats::setNames(as.numeric(out), groups)
  }

  # ---- normalize args ---------------------------------------------------------

  group_cols <- .hydro_erfa_normalize_cols(group_cols)
  scenario_cols <- .hydro_erfa_normalize_cols(scenario_cols)

  # group_cols apply to both; scenario_cols apply only to scenario_tbl
  if (!is.null(group_cols) && any(group_cols %in% c("metric_id", "value"))) {
    stop("group_cols must not include 'metric_id' or 'value'.", call. = FALSE)
  }
  if (!is.null(scenario_cols) && any(scenario_cols %in% c("metric_id", "value"))) {
    stop("scenario_cols must not include 'metric_id' or 'value'.", call. = FALSE)
  }
  if (!is.null(group_cols) && !is.null(scenario_cols) && length(intersect(group_cols, scenario_cols)) > 0L) {
    stop("group_cols and scenario_cols must not overlap.", call. = FALSE)
  }

  # ---- validate inputs --------------------------------------------------------

  .hydro_erfa_validate_cols(baseline_tbl, "baseline_tbl", c("metric_id", "value", group_cols))
  .hydro_erfa_validate_cols(scenario_tbl, "scenario_tbl", c("metric_id", "value", group_cols, scenario_cols))

  baseline_tbl$metric_id <- as.integer(baseline_tbl$metric_id)
  scenario_tbl$metric_id <- as.integer(scenario_tbl$metric_id)

  if (anyNA(baseline_tbl$metric_id)) stop("baseline_tbl$metric_id contains NA after coercion.", call. = FALSE)
  if (anyNA(scenario_tbl$metric_id)) stop("scenario_tbl$metric_id contains NA after coercion.", call. = FALSE)

  metric_def <- .hydro_erfa_resolve_metric_def(metric_set)
  sensi_by_group <- .hydro_erfa_resolve_sensi_thresholds(sensi_threshold)

  # ---- enforce "compare each scenario to baseline" semantics -------------------
  # 1) only keep scenario group-combos that exist in baseline (do not invent baseline groups)
  if (!is.null(group_cols) && length(group_cols) > 0L) {
    base_groups <- dplyr::distinct(baseline_tbl, dplyr::across(dplyr::all_of(group_cols)))
    scenario_tbl <- dplyr::semi_join(scenario_tbl, base_groups, by = group_cols)
  }

  # 2) restrict both tables to metric_ids defined by metric_set (authoritative)
  baseline_tbl <- dplyr::semi_join(baseline_tbl, metric_def[, c("metric_id")], by = "metric_id")
  scenario_tbl <- dplyr::semi_join(scenario_tbl, metric_def[, c("metric_id")], by = "metric_id")

  # 3) attach registry metadata (group, metric_key, stat, metric)
  base_f <- dplyr::inner_join(
    baseline_tbl |>
      dplyr::select(dplyr::all_of(c(group_cols, "metric_id", "value"))) |>
      dplyr::rename(value_base = value),
    metric_def,
    by = "metric_id"
  )

  scen_f <- dplyr::inner_join(
    scenario_tbl |>
      dplyr::select(dplyr::all_of(c(group_cols, scenario_cols, "metric_id", "value"))) |>
      dplyr::rename(value_scen = value),
    metric_def,
    by = "metric_id"
  )

  # 4) broadcast baseline across scenario_cols by joining baseline onto scenario rows
  join_keys <- c(group_cols, "metric_id")
  df <- dplyr::inner_join(
    scen_f,
    base_f |>
      dplyr::select(dplyr::all_of(c(join_keys, "value_base"))),
    by = join_keys
  ) |>
    dplyr::mutate(
      pct_change = dplyr::if_else(
        is.finite(value_base) & value_base != 0,
        100 * (value_scen - value_base) / value_base,
        NA_real_
      ),
      sensi_threshold_group = as.numeric(sensi_by_group[as.character(.data$group)]),
      outside_range = abs(pct_change) > sensi_threshold_group
    )

  # ---- summaries --------------------------------------------------------------

  # Per ERFA group within each group_cols × scenario_cols combination
  summary_keys <- c(group_cols, scenario_cols, "group")

  summary_tbl <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_keys))) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_outside = sum(outside_range, na.rm = TRUE),
      perc_outside = 100 * n_outside / n_total,
      .groups = "drop"
    )

  # Overall row within each group_cols × scenario_cols combination
  overall_tbl <- summary_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, scenario_cols)))) |>
    dplyr::summarise(
      group = "Overall",
      n_total = sum(n_total),
      n_outside = sum(n_outside),
      perc_outside = 100 * n_outside / n_total,
      .groups = "drop"
    )

  summary_tbl <- dplyr::bind_rows(summary_tbl, overall_tbl)

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
      ),
      group = factor(group, levels = c("HF", "MF", "LF", "RFC", "IF", "Overall"), ordered = TRUE)
    ) |>
    dplyr::select(dplyr::all_of(c(group_cols, scenario_cols)), group, n_total, n_outside, perc_outside, risk_class, risk_label) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, scenario_cols))), group)

  list(detail = df, summary = summary_tbl)
}
