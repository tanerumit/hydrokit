#' List available metric metadata across families
#'
#' @description
#' Returns a unified metadata table of available metrics across flow,
#' water-resources, and drought families.
#'
#' @details
#' The result is sourced from internal family catalogs and includes metadata
#' columns only. Runtime registry fields such as compute handlers are excluded.
#'
#' @param family Optional character scalar. One of
#'   \code{"flow"}, \code{"water_resources"}, or \code{"drought"}.
#'   If \code{NULL}, all families are returned.
#'
#' @return A tibble with columns:
#'   \code{key}, \code{family}, \code{group}, \code{grain}, \code{label}, \code{input}.
#'
#' @examples
#' list_metrics()
#' list_metrics(family = "flow")
#'
#' @export
list_metrics <- function(family = NULL) {
  out <- dplyr::bind_rows(
    .metric_catalog_flow(),
    .metric_catalog_water_resources(),
    .metric_catalog_drought()
  )

  if (anyDuplicated(out$key) > 0L) {
    dup_keys <- unique(out$key[duplicated(out$key)])
    stop(
      "Duplicate metric keys across catalogs are not supported: ",
      paste(dup_keys, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (is.null(family)) {
    return(out)
  }

  if (!is.character(family) || length(family) != 1L || is.na(family) || !nzchar(family)) {
    stop("family must be NULL or one of: flow, water_resources, drought.", call. = FALSE)
  }

  keep_families <- c("flow", "water_resources", "drought")
  if (!family %in% keep_families) {
    stop("Unknown family: ", family, ". Use one of: ", paste(keep_families, collapse = ", "), ".", call. = FALSE)
  }

  out[out$family == family, , drop = FALSE]
}
