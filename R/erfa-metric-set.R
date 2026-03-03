# ==============================================================================
# Script: R/erfa-metric-set.R
# Purpose: ERFA metric-set descriptors and dispatcher.
# ==============================================================================

#' ERFA70 metric-set descriptor
#'
#' @return A list with components:
#'   name, keys, and metadata (key/group/grain/label).
#' @keywords internal
.metric_set_erfa70 <- function() {
  keys <- c(
    "amax1d", "amax3d", "amax7d", "amax30d", "amax90d",
    "juldate_amax1d",
    "num_high_pulses", "avg_dur_high_pulses",
    "january_flow", "february_flow", "march_flow", "april_flow", "may_flow", "june_flow",
    "july_flow", "august_flow", "september_flow", "october_flow", "november_flow", "december_flow",
    "amin1d", "amin3d", "amin7d", "amin30d", "amin90d",
    "juldate_amin1d",
    "num_low_pulses", "avg_dur_low_pulses",
    "mean_rate_rise", "mean_rate_fall", "num_rises", "num_falls",
    "num_zero_seq", "avg_dur_zero_seq", "time_main_zero_seq"
  )

  cat_flow <- .metric_catalog_flow()
  idx <- match(keys, cat_flow$key)
  if (anyNA(idx)) {
    stop("ERFA70 metric set references unknown flow keys.", call. = FALSE)
  }

  list(
    name = "erfa70",
    keys = keys,
    metadata = cat_flow[idx, c("key", "group", "grain", "label"), drop = FALSE]
  )
}

#' Resolve named metric set descriptor
#'
#' @param metric_set Character scalar metric-set name.
#' @return A metric-set descriptor list.
#' @keywords internal
.resolve_metric_set <- function(metric_set) {
  if (!is.character(metric_set) || length(metric_set) != 1L || is.na(metric_set) || !nzchar(metric_set)) {
    stop("metric_set must be a non-empty character scalar.", call. = FALSE)
  }
  if (identical(metric_set, "erfa70")) {
    return(.metric_set_erfa70())
  }
  stop("Unknown metric_set: ", metric_set, call. = FALSE)
}

#' Expand ERFA70 set to legacy med/iqr metric mapping
#'
#' @return Tibble mapping metric_id, group, metric_key, stat, and metric.
#' @keywords internal
.flow_registry_erfa70_expanded <- function() {
  set <- .metric_set_erfa70()
  stat_vec <- rep(c("med", "iqr"), times = length(set$keys))
  metric_key_vec <- rep(set$keys, each = 2L)
  group_vec <- rep(set$metadata$group, each = 2L)
  suffix <- ifelse(stat_vec == "med", "m", "v")

  tibble::tibble(
    metric_id = seq_len(length(metric_key_vec)),
    group = factor(group_vec, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
    metric_key = metric_key_vec,
    stat = stat_vec,
    metric = paste0(metric_key_vec, "_", suffix)
  )
}
