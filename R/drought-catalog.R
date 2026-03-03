# ==============================================================================
# Script: R/drought-catalog.R
# Purpose: Internal metadata catalog for drought metrics.
# ==============================================================================

#' Internal drought metric catalog
#'
#' @return Tibble with metadata columns:
#'   key, family, group, grain, label, input.
#' @keywords internal
.metric_catalog_drought <- function() {
  tibble::tibble(
    key = c("max_dry_spell", "compute_dpi", "calculate_spei", "calculate_spi"),
    family = "drought",
    group = c("spell", "index", "index", "index"),
    grain = c("daily", "series", "monthly", "monthly"),
    label = c(
      "Maximum dry-spell duration",
      "Drought probability index",
      "Standardized precipitation evapotranspiration index",
      "Standardized precipitation index"
    ),
    input = c("P", "spei_values", "P,PET", "P")
  )
}
