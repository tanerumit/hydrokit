# ------------------------------------------------------------------------------
# SUMMARY FUNCTIONS RESOLVER
# ------------------------------------------------------------------------------

#' Resolve summary statistic functions from labels or a named list.
#' @param summaries Character vector of summary names or a named list of functions.
#' @return Named list of summary functions.
#' @keywords internal
.flow_resolve_summaries <- function(summaries = c("med", "iqr")) {
  if (is.character(summaries)) {
    out <- list()
    for (nm in summaries) {
      if (identical(nm, "med")) {
        out[["med"]] <- function(x) stats::median(x, na.rm = TRUE)
      } else if (identical(nm, "iqr")) {
        out[["iqr"]] <- function(x) stats::IQR(x, na.rm = TRUE)
      } else {
        stop("Unknown summary: ", nm, ". Supported: 'med', 'iqr'.", call. = FALSE)
      }
    }
    return(out)
  }

  if (is.list(summaries) && length(summaries) > 0L && !is.null(names(summaries))) {
    return(summaries)
  }

  stop("summaries must be a character vector (e.g., c('med','iqr')) or a named list of functions.",
       call. = FALSE)
}

.hydro_resolve_summaries <- .flow_resolve_summaries


# ==============================================================================
# EXPORTED: summarize_hydro_metrics()
# ==============================================================================

#' Summarize Annual Hydrological Metrics Across Years
#'
#' @description
#' Takes the output of \code{\link{calculate_hydro_metrics}} and applies summary
#' statistics across years for annual-grain metrics. Optionally maps ERFA metric
#' IDs when the full ERFA70 set and standard summaries are used.
#'
#' @param annual_tbl Tibble as returned by \code{\link{calculate_hydro_metrics}}.
#'   Must contain columns: \code{year}, \code{metric_key}, \code{group},
#'   \code{grain}, \code{value}. Optionally \code{site}.
#' @param summaries Character vector of summary stat names (e.g. \code{c("med", "iqr")})
#'   or a named list of functions.
#'
#' @return A tibble with columns:
#'   \code{metric_id} (ERFA legacy ID when applicable, else NA),
#'   \code{metric_key}, \code{group}, \code{stat}, \code{metric}
#'   (legacy name, e.g. "amax1d_m"), \code{value}.
#'   For multi-site input, includes \code{site}.
#'
#' @seealso \code{\link{calculate_hydro_metrics}}, \code{\link{evaluate_erfa_class}}
#'
#' @export
#'
#' @examples
#' date <- seq.Date(as.Date("2001-01-01"), as.Date("2005-12-31"), by = "day")
#' Q <- 2 + sin(seq_along(date) * 2 * pi / 365)
#' annual <- calculate_hydro_metrics(Q, date)
#' summary_tbl <- summarize_hydro_metrics(annual)
#' summary_tbl
summarize_hydro_metrics <- function(
    annual_tbl,
    summaries = c("med", "iqr")
) {

  # ---- validate input
  req_cols <- c("year", "metric_key", "group", "grain", "value")
  miss <- setdiff(req_cols, names(annual_tbl))
  if (length(miss) > 0L) {
    stop("annual_tbl is missing required columns: ", paste(miss, collapse = ", "),
         ". Did you pass the output of calculate_hydro_metrics()?", call. = FALSE)
  }

  has_site <- "site" %in% names(annual_tbl)
  summary_fns <- .flow_resolve_summaries(summaries)
  stat_names <- names(summary_fns)

  # Only summarize annual-grain metrics
  annual_data <- annual_tbl[annual_tbl$grain == "annual", , drop = FALSE]

  # Period-grain metrics pass through as-is (already a single value)
  period_data <- annual_tbl[annual_tbl$grain == "period", , drop = FALSE]

  if (nrow(annual_data) == 0L && nrow(period_data) == 0L) {
    return(tibble::tibble(
      metric_id = integer(0), metric_key = character(0),
      group = factor(character(0), levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
      stat = character(0), metric = character(0), value = numeric(0)
    ))
  }

  # ---- Summarize annual-grain metrics
  .summarize_group <- function(df) {
    metric_keys <- unique(df$metric_key)
    reg <- .flow_metric_registry()
    reg <- reg[match(metric_keys, reg$metric_key), , drop = FALSE]
    reg$group <- factor(reg$group, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE)

    sum_list <- vector("list", length(stat_names))
    for (si in seq_along(stat_names)) {
      st <- stat_names[si]
      fn <- summary_fns[[st]]
      vals <- numeric(length(metric_keys))
      for (k in seq_along(metric_keys)) {
        x <- df$value[df$metric_key == metric_keys[k]]
        vals[k] <- fn(x)
      }
      suffix <- if (identical(st, "med")) "m" else if (identical(st, "iqr")) "v" else st
      sum_list[[si]] <- tibble::tibble(
        metric_key = metric_keys,
        group = reg$group,
        stat = st,
        metric = paste0(metric_keys, "_", suffix),
        value = as.numeric(vals)
      )
    }
    dplyr::bind_rows(sum_list)
  }

  if (has_site) {
    sites <- unique(annual_data$site)
    out_list <- vector("list", length(sites))
    for (i in seq_along(sites)) {
      s <- sites[i]
      df_s <- annual_data[annual_data$site == s, , drop = FALSE]
      res <- .summarize_group(df_s)
      res$site <- s
      out_list[[i]] <- res
    }
    summary_tbl <- dplyr::bind_rows(out_list)
    summary_tbl <- dplyr::relocate(summary_tbl, site, .before = metric_key)
  } else {
    summary_tbl <- .summarize_group(annual_data)
  }

  # ---- Append period-grain metrics (one row per stat, value repeated)
  if (nrow(period_data) > 0L) {
    period_summary_parts <- vector("list", length(stat_names))
    for (si in seq_along(stat_names)) {
      st <- stat_names[si]
      suffix <- if (identical(st, "med")) "m" else if (identical(st, "iqr")) "v" else st
      p <- period_data[, c(if (has_site) "site", "metric_key", "group", "value"), drop = FALSE]
      p$stat <- st
      p$metric <- paste0(p$metric_key, "_", suffix)
      period_summary_parts[[si]] <- p
    }
    period_tbl <- dplyr::bind_rows(period_summary_parts)
    summary_tbl <- dplyr::bind_rows(summary_tbl, period_tbl)
  }

  # ---- ERFA metric_id mapping (only for full ERFA70 + med/iqr)
  metric_keys_used <- unique(summary_tbl$metric_key)
  is_erfa_set <- identical(sort(metric_keys_used), sort(.flow_metric_sets()[["erfa70"]]))
  has_erfa_stats <- identical(sort(stat_names), sort(c("med", "iqr")))

  if (is_erfa_set && has_erfa_stats) {
    erfa70 <- .flow_registry_erfa70_expanded()
    summary_tbl <- dplyr::left_join(
      summary_tbl, erfa70,
      by = c("metric_key", "stat", "group", "metric")
    )
    summary_tbl <- dplyr::relocate(summary_tbl, metric_id, .before = metric_key)
  } else {
    summary_tbl$metric_id <- NA_integer_
    summary_tbl <- dplyr::relocate(summary_tbl, metric_id, .before = metric_key)
  }

  summary_tbl
}

#' Summarize flow metrics across years
#'
#' @description
#' Flow-named API alias for \code{\link{summarize_hydro_metrics}}.
#'
#' @details
#' This function forwards all arguments to
#' \code{\link{summarize_hydro_metrics}} and returns the same structure.
#'
#' @inheritParams summarize_hydro_metrics
#'
#' @return A tibble with columns:
#'   \code{metric_id}, \code{metric_key}, \code{group}, \code{stat},
#'   \code{metric}, \code{value}, and optional \code{site}.
#'
#' @examples
#' date <- seq.Date(as.Date("2001-01-01"), as.Date("2005-12-31"), by = "day")
#' Q <- 2 + sin(seq_along(date) * 2 * pi / 365)
#' annual <- calculate_flow_metrics(Q, date)
#' summarize_flow_metrics(annual)
#'
#' @export
summarize_flow_metrics <- function(
    annual_tbl,
    summaries = c("med", "iqr")
) {
  summarize_hydro_metrics(annual_tbl = annual_tbl, summaries = summaries)
}
