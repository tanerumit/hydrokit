# ==============================================================================
# Script: R/wr-catalog.R
# Purpose: Internal metadata catalog for water-resources metrics.
# ==============================================================================

#' Internal water-resources metric catalog
#'
#' @return Tibble with metadata columns:
#'   key, family, group, grain, label, input.
#' @keywords internal
.metric_catalog_water_resources <- function() {
  tibble::tibble(
    key = c(
      "reliability_hashimoto",
      "resilience_hashimoto",
      "vulnerability_hashimoto",
      "failure_frequency",
      "mean_failure_duration",
      "mean_recovery_time",
      "vulnerability_event_hashimoto",
      "extract_state_hashimoto",
      "failure_metrics_hashimoto"
    ),
    family = "water_resources",
    group = "performance",
    grain = "timestep",
    label = c(
      "Hashimoto reliability",
      "Hashimoto resilience",
      "Hashimoto vulnerability",
      "Failure frequency",
      "Mean failure duration",
      "Mean recovery time",
      "Event-based Hashimoto vulnerability",
      "Extract Hashimoto state components",
      "Hashimoto failure diagnostics summary"
    ),
    input = "obs_threshold,sim_supply"
  )
}
