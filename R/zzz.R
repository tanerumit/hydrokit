# ==============================================================================
# Script: R/zzz.R
# Purpose: Package load-time hooks.
# ==============================================================================

.onLoad <- function(libname, pkgname) {

  # Validate the metric registry once at package load rather than on every call

  # to calculate_hydro_metrics(). Failures here indicate a packaging bug, not a
  # user error, so we surface them early.
  tryCatch(
    .hydro_validate_registry(strict = TRUE),
    error = function(e) {
      warning(
        "hydrokit: metric registry validation failed on load: ", conditionMessage(e),
        "\nMetric computation may behave unexpectedly. ",
        "Please report this issue.",
        call. = FALSE
      )
    }
  )
}
