# ==============================================================================
# Script: R/flow-catalog.R
# Purpose: Internal metadata catalog for flow metrics.
# ==============================================================================

#' Internal flow metric catalog
#'
#' @return Tibble with metadata columns:
#'   key, family, group, grain, label, input.
#' @keywords internal
.metric_catalog_flow <- function() {
  tibble::tibble(
    key = c(
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
    ),
    family = "flow",
    group = c(
      rep("HF", 8),
      rep("MF", 12),
      rep("LF", 8),
      rep("RFC", 4),
      rep("IF", 3)
    ),
    grain = rep("annual", 35),
    label = c(
      "Annual 1-day maximum flow",
      "Annual 3-day rolling-mean maximum flow",
      "Annual 7-day rolling-mean maximum flow",
      "Annual 30-day rolling-mean maximum flow",
      "Annual 90-day rolling-mean maximum flow",
      "Julian day of annual 1-day maximum flow",
      "Number of high-flow pulses",
      "Average duration of high-flow pulses (days)",
      "Mean January flow",
      "Mean February flow",
      "Mean March flow",
      "Mean April flow",
      "Mean May flow",
      "Mean June flow",
      "Mean July flow",
      "Mean August flow",
      "Mean September flow",
      "Mean October flow",
      "Mean November flow",
      "Mean December flow",
      "Annual 1-day minimum flow",
      "Annual 3-day rolling-mean minimum flow",
      "Annual 7-day rolling-mean minimum flow",
      "Annual 30-day rolling-mean minimum flow",
      "Annual 90-day rolling-mean minimum flow",
      "Julian day of annual 1-day minimum flow",
      "Number of low-flow pulses",
      "Average duration of low-flow pulses (days)",
      "Mean rate of rise",
      "Mean rate of fall",
      "Number of rising days",
      "Number of falling days",
      "Number of zero-flow sequences",
      "Average duration of zero-flow sequences (days)",
      "Julian day of start of longest zero-flow sequence"
    ),
    input = "Q"
  )
}
