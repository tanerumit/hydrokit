# ------------------------------------------------------------------------------
# INTERNAL HELPERS
# ------------------------------------------------------------------------------

#' Validate daily date ordering, uniqueness, and regular spacing.
#' @param datev Date vector to validate.
#' @return Invisible TRUE.
#' @keywords internal
.hydro_validate_daily_dates <- function(datev) {
  if (length(datev) == 0L) return(invisible(TRUE))

  ord <- order(datev)
  if (!identical(ord, seq_along(datev))) {
    stop("date must be sorted in increasing order.", call. = FALSE)
  }
  if (anyDuplicated(datev) > 0L) {
    stop("date contains duplicated values; daily metrics require unique dates.", call. = FALSE)
  }
  dd <- as.integer(diff(datev))
  if (length(dd) > 0L && any(dd != 1L)) {
    warning(
      "date has gaps (diff != 1 day). Rolling means and rise/fall counts may be biased.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Right-aligned rolling mean with NA padding.
#' @keywords internal
.hydro_rollmean_right <- function(x, k) {
  k <- as.integer(k)
  if (length(x) == 0L) return(numeric(0))
  if (k <= 1L) return(as.numeric(x))
  zoo::rollmean(x, k, align = "right", fill = NA)
}

#' Centered rolling mean with NA padding.
#' @keywords internal
.hydro_smooth_center <- function(x, k) {
  k <- as.integer(k)
  if (length(x) == 0L) return(numeric(0))
  if (k <= 1L) return(as.numeric(x))
  zoo::rollmean(x, k, align = "center", fill = NA)
}

# ------------------------------------------------------------------------------
# INTERNAL: Resolve zero threshold from consolidated argument
# ------------------------------------------------------------------------------

#' Parse the consolidated zero_threshold argument.
#'
#' Accepts a numeric scalar (absolute mode) or a named list with 'mode' and 'value'.
#' Returns a standardized list with mode, value, and optional extra params.
#'
#' @param zero_threshold Numeric scalar or list.
#' @return List with elements: mode, value.
#' @keywords internal
.hydro_parse_zero_threshold <- function(zero_threshold) {
  if (is.numeric(zero_threshold) && length(zero_threshold) == 1L) {
    if (!is.finite(zero_threshold)) {
      stop("zero_threshold must be finite.", call. = FALSE)
    }
    return(list(mode = "absolute", value = zero_threshold))
  }

  if (is.list(zero_threshold)) {
    mode <- zero_threshold$mode
    value <- zero_threshold$value
    if (is.null(mode) || is.null(value)) {
      stop("zero_threshold list must have 'mode' and 'value' elements.", call. = FALSE)
    }
    allowed <- c("absolute", "quantile", "fraction_median")
    if (!mode %in% allowed) {
      stop("zero_threshold$mode must be one of: ",
           paste(allowed, collapse = ", "), ".", call. = FALSE)
    }
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("zero_threshold$value must be a finite numeric scalar.", call. = FALSE)
    }
    if (mode == "quantile" && (value < 0 || value > 1)) {
      stop("zero_threshold$value must be in [0,1] for mode='quantile'.", call. = FALSE)
    }
    if (mode == "fraction_median" && value < 0) {
      stop("zero_threshold$value must be >= 0 for mode='fraction_median'.", call. = FALSE)
    }
    return(list(mode = mode, value = value))
  }

  stop("zero_threshold must be a numeric scalar or a list with 'mode' and 'value'.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# INTERNAL: Resolve baseline from consolidated argument
# ------------------------------------------------------------------------------

#' Parse the consolidated baseline argument.
#'
#' Accepts an integer vector (years), a length-2 Date vector (date range), or NULL.
#'
#' @param baseline Integer vector, Date vector, or NULL.
#' @param datev Date vector for overlap checks.
#' @return List with elements: years (integer or NULL), dates (Date or NULL).
#' @keywords internal
.hydro_parse_baseline <- function(baseline, datev) {
  if (is.null(baseline)) return(list(years = NULL, dates = NULL))

  # Date range
  if (inherits(baseline, "Date")) {
    if (length(baseline) != 2L || anyNA(baseline)) {
      stop("baseline as Date must be length 2: c(start, end).", call. = FALSE)
    }
    if (baseline[1] > baseline[2]) {
      stop("baseline dates must be in increasing order.", call. = FALSE)
    }
    if (baseline[2] < min(datev) || baseline[1] > max(datev)) {
      stop("baseline dates do not overlap the provided date range.", call. = FALSE)
    }
    return(list(years = NULL, dates = baseline))
  }

  # Integer years
  if (is.numeric(baseline)) {
    baseline <- as.integer(baseline)
    if (anyNA(baseline)) {
      stop("baseline years must not contain NA.", call. = FALSE)
    }
    return(list(years = baseline, dates = NULL))
  }

  stop("baseline must be an integer vector (years), a length-2 Date vector, or NULL.",
       call. = FALSE)
}

# ------------------------------------------------------------------------------
# INTERNAL: compute high/low thresholds
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_compute_hilo_thresholds <- function(Qv, high_q, low_q, min_finite) {
  q <- Qv[is.finite(Qv)]
  if (length(q) < min_finite) {
    stop("Insufficient finite discharge values (", length(q),
         ") to compute thresholds; need at least ", min_finite, ".", call. = FALSE)
  }
  high_thr <- stats::quantile(q, probs = high_q, na.rm = TRUE, names = FALSE)
  low_thr  <- stats::quantile(q, probs = low_q,  na.rm = TRUE, names = FALSE)
  if (!is.finite(high_thr) || !is.finite(low_thr)) {
    stop("Computed high/low thresholds are not finite.", call. = FALSE)
  }
  list(high_thr = high_thr, low_thr = low_thr)
}

# ------------------------------------------------------------------------------
# INTERNAL: compute zero threshold (uses parsed zero_threshold)
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_compute_zero_threshold <- function(Qv, zero_parsed, min_finite) {
  mode <- zero_parsed$mode
  value <- zero_parsed$value

  if (identical(mode, "absolute")) return(value)

  q <- Qv[is.finite(Qv)]
  if (length(q) < min_finite) {
    stop("Insufficient finite discharge values (", length(q),
         ") to compute zero threshold; need at least ", min_finite, ".", call. = FALSE)
  }

  if (identical(mode, "quantile")) {
    z <- stats::quantile(q, probs = value, na.rm = TRUE, names = FALSE)
    if (!is.finite(z)) stop("Computed zero threshold is not finite.", call. = FALSE)
    return(z)
  }

  if (identical(mode, "fraction_median")) {
    z <- value * stats::median(q, na.rm = TRUE)
    if (!is.finite(z)) stop("Computed zero threshold is not finite.", call. = FALSE)
    return(z)
  }

  stop("Unknown zero threshold mode: ", mode, call. = FALSE)
}

# ------------------------------------------------------------------------------
# INTERNAL: build threshold object
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_build_threshold_object <- function(
    Qv, datev, threshold_scope,
    high_q, low_q, zero_parsed, min_finite,
    baseline_parsed
) {

  if (identical(threshold_scope, "annual")) {
    return(list(scope = "annual"))
  }

  if (identical(threshold_scope, "global")) {
    hl <- .hydro_compute_hilo_thresholds(Qv, high_q, low_q, min_finite)
    z  <- .hydro_compute_zero_threshold(Qv, zero_parsed, min_finite)
    return(list(scope = "global", high = hl$high_thr, low = hl$low_thr, zero = z))
  }

  if (identical(threshold_scope, "baseline")) {
    yr <- lubridate::year(datev)
    keep <- rep(TRUE, length(datev))
    if (!is.null(baseline_parsed$years)) {
      keep <- keep & (yr %in% baseline_parsed$years)
    }
    if (!is.null(baseline_parsed$dates)) {
      bd <- baseline_parsed$dates
      keep <- keep & (datev >= bd[1] & datev <= bd[2])
    }
    qsub <- Qv[keep]
    hl <- .hydro_compute_hilo_thresholds(qsub, high_q, low_q, min_finite)
    z  <- .hydro_compute_zero_threshold(qsub, zero_parsed, min_finite)
    return(list(scope = "baseline", high = hl$high_thr, low = hl$low_thr, zero = z))
  }

  if (identical(threshold_scope, "seasonal")) {
    mon <- lubridate::month(datev)
    high <- rep(NA_real_, 12L)
    low  <- rep(NA_real_, 12L)
    zero <- rep(NA_real_, 12L)
    for (m in 1:12) {
      qsub <- Qv[which(mon == m)]
      hl <- .hydro_compute_hilo_thresholds(qsub, high_q, low_q, min_finite)
      z  <- .hydro_compute_zero_threshold(qsub, zero_parsed, min_finite)
      high[m] <- hl$high_thr
      low[m]  <- hl$low_thr
      zero[m] <- z
    }
    names(high) <- as.character(1:12)
    names(low)  <- as.character(1:12)
    names(zero) <- as.character(1:12)
    return(list(scope = "seasonal", high = high, low = low, zero = zero))
  }

  stop("Unknown threshold_scope: ", threshold_scope, call. = FALSE)
}

# ------------------------------------------------------------------------------
# INTERNAL: annual context with lazy caching
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_annual_context <- function(
    Qy, jdy, mony, y,
    thr_high_vec, thr_low_vec, thr_zero_vec,
    ctrl
) {
  cache <- new.env(parent = emptyenv())

  get <- function(name) {
    if (exists(name, envir = cache, inherits = FALSE)) {
      return(base::get(name, envir = cache, inherits = FALSE))
    }

    val <- switch(name,
                  r3  = .hydro_rollmean_right(Qy, 3L),
                  r7  = .hydro_rollmean_right(Qy, 7L),
                  r30 = .hydro_rollmean_right(Qy, 30L),
                  r90 = .hydro_rollmean_right(Qy, 90L),
                  hp  = .hydro_seq_stats(Qy > thr_high_vec),
                  lp  = .hydro_seq_stats(Qy < thr_low_vec),
                  zp  = .hydro_seq_stats(Qy <= thr_zero_vec),
                  mm  = .hydro_monthly_means(Qy, y = y, mony = mony,
                                             month_min_frac = ctrl$month_min_frac),
                  roc = {
                    Qroc <- .hydro_smooth_center(Qy, ctrl$roc_smooth_k)
                    dQy <- diff(Qroc)
                    eps <- ctrl$roc_eps
                    list(
                      dQ = dQy,
                      rise = dQy[dQy > eps],
                      fall = abs(dQy[dQy < -eps]),
                      n_rise = sum(dQy > eps, na.rm = TRUE),
                      n_fall = sum(dQy < -eps, na.rm = TRUE)
                    )
                  },
                  stop("Unknown context key: ", name, call. = FALSE)
    )

    assign(name, val, envir = cache)
    val
  }

  list(Qy = Qy, jdy = jdy, mony = mony, y = y,
       thr_high_vec = thr_high_vec, thr_low_vec = thr_low_vec,
       thr_zero_vec = thr_zero_vec, ctrl = ctrl, get = get)
}

# ------------------------------------------------------------------------------
# INTERNAL: monthly means with completeness enforcement
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_monthly_means <- function(Qy, y, mony, month_min_frac = 0) {
  m_mean <- rep(NA_real_, 12L)
  for (m in 1:12) {
    idx <- which(mony == m)
    if (length(idx) == 0L) next
    first_day <- as.Date(sprintf("%04d-%02d-01", y, m))
    expected <- as.integer(lubridate::days_in_month(first_day))
    obs <- sum(is.finite(Qy[idx]))
    frac <- if (expected > 0L) obs / expected else NA_real_
    if (!is.finite(frac) || frac < month_min_frac) next
    v <- mean(Qy[idx], na.rm = TRUE)
    m_mean[m] <- if (is.nan(v)) NA_real_ else v
  }
  list(mean = m_mean)
}

# ==============================================================================
# EXPORTED: hydro_control()
# ==============================================================================

#' Control parameters for hydrological metric computation
#'
#' Creates a validated control object grouping expert-level tuning parameters
#' that rarely need changing from their defaults.
#'
#' @param roc_eps Numeric scalar >= 0. Deadband for rise/fall classification;
#'   daily changes with absolute magnitude <= roc_eps are ignored. Default 0.
#' @param roc_smooth_k Integer scalar >= 1. Centered moving-average window (days)
#'   applied before rate-of-change calculations. 1 disables smoothing.
#' @param month_min_frac Numeric scalar in \code{[0,1]}. Minimum fraction of calendar
#'   days in a month that must have finite discharge to compute that monthly mean.
#'   Default 0.8.
#' @param min_finite_thresholds Integer scalar >= 1. Minimum number of finite values
#'   required to estimate flow thresholds within each subset.
#'
#' @return A list of class \code{hydro_control}.
#' @export
#'
#' @examples
#' # Default control
#' hydro_control()
#'
#' # Stricter monthly completeness
#' hydro_control(month_min_frac = 0.9)
hydro_control <- function(
    roc_eps = 0,
    roc_smooth_k = 1L,
    month_min_frac = 0.8,
    min_finite_thresholds = 10L
) {
  if (!is.numeric(roc_eps) || length(roc_eps) != 1L || is.na(roc_eps) || roc_eps < 0) {
    stop("roc_eps must be a numeric scalar >= 0.", call. = FALSE)
  }
  roc_smooth_k <- as.integer(roc_smooth_k)
  if (is.na(roc_smooth_k) || roc_smooth_k < 1L) {
    stop("roc_smooth_k must be an integer >= 1.", call. = FALSE)
  }
  if (!is.numeric(month_min_frac) || length(month_min_frac) != 1L || is.na(month_min_frac) ||
      month_min_frac < 0 || month_min_frac > 1) {
    stop("month_min_frac must be a numeric scalar in [0,1].", call. = FALSE)
  }
  min_finite_thresholds <- as.integer(min_finite_thresholds)
  if (is.na(min_finite_thresholds) || min_finite_thresholds < 1L) {
    stop("min_finite_thresholds must be an integer >= 1.", call. = FALSE)
  }

  out <- list(
    roc_eps = roc_eps,
    roc_smooth_k = roc_smooth_k,
    month_min_frac = month_min_frac,
    min_finite_thresholds = min_finite_thresholds
  )
  class(out) <- c("hydro_control", "list")
  out
}

#' Flow metric options constructor
#'
#' @description
#' Creates a validated options object for flow metric computation.
#'
#' @details
#' This is an alias of \code{\link{hydro_control}} maintained for flow-named API
#' consistency. It returns an object of class \code{hydro_control}.
#'
#' @inheritParams hydro_control
#'
#' @return A list of class \code{hydro_control}.
#'
#' @examples
#' flow_metric_options()
#'
#' @export
flow_metric_options <- hydro_control

# Backward-compatible non-exported alias for historical discovery helper.
list_hydro_metrics <- function(purpose = NULL) {
  out <- list_metrics(family = "flow")
  if (is.null(purpose)) {
    return(out[, c("key", "group", "grain", "label"), drop = FALSE])
  }

  reg <- .flow_metric_registry()
  purpose <- as.character(purpose)[1]
  keep <- vapply(reg$purposes, function(x) purpose %in% x, logical(1))
  out <- out[keep, c("key", "group", "grain", "label"), drop = FALSE]
  if (nrow(out) == 0L) {
    warning("No metrics found for purpose: ", purpose, call. = FALSE)
  }
  out
}

# ==============================================================================
# EXPORTED: calculate_hydro_metrics()
# ==============================================================================

#' Calculate Hydrological Metrics from Daily Discharge
#'
#' @description
#' Computes registry-defined hydrological metrics from daily discharge data.
#' Returns values at each metric's natural temporal grain (annual or period-level)
#' as defined by the metric registry.
#'
#' @details
#' \strong{Metric selection:} The \code{metrics} argument accepts:
#' \itemize{
#'   \item A registered set name (e.g. \code{"erfa70"}) â€” selects the predefined set
#'   \item A character vector of metric keys (e.g. \code{c("amax1d", "amin7d")})
#'   \item \code{NULL} (default) â€” uses the \code{"erfa70"} set
#' }
#' Use \code{\link{list_metrics}} to see available keys.
#'
#' \strong{Water year:} By default, data is grouped into water years starting in
#' October (\code{water_year_start = 10}), following IHA convention. Set to 1
#' for calendar years.
#'
#' \strong{Zero-flow threshold:} Accepts either a numeric scalar (absolute mode,
#' e.g. \code{0.02}) or a list for relative modes:
#' \code{list(mode = "fraction_median", value = 0.05)} or
#' \code{list(mode = "quantile", value = 0.02)}.
#'
#' \strong{Baseline:} When \code{threshold_scope = "baseline"}, pass an integer
#' vector of years or a length-2 Date vector as \code{baseline}.
#'
#' @param Q Numeric vector (single site) or matrix/data.frame (multi-site) of
#'   daily discharge values.
#' @param date Date vector corresponding to rows of \code{Q}.
#' @param metrics Character vector of metric keys, a registered set name
#'   (e.g. "erfa70"), or NULL for the default set.
#' @param water_year_start Integer 1..12. Month that begins the water year.
#'   Default 10 (October).
#' @param threshold_scope Character scalar: "global", "annual", "seasonal", or
#'   "baseline".
#' @param baseline Integer vector (years) or length-2 Date vector for baseline
#'   threshold estimation. Only used when \code{threshold_scope = "baseline"}.
#' @param high_thresh_quantile Numeric in (0,1). Quantile for high-flow pulse
#'   threshold.
#' @param low_thresh_quantile Numeric in (0,1). Quantile for low-flow pulse
#'   threshold.
#' @param zero_threshold Numeric scalar (absolute) or list with \code{mode} and
#'   \code{value} for zero/cessation flow threshold.
#' @param control A \code{\link{hydro_control}} object for expert parameters.
#'
#' @return A tibble with columns:
#'   \code{year} (water year; NA for period-grain metrics),
#'   \code{metric_key}, \code{group}, \code{grain}, \code{value}.
#'   For multi-site input, includes \code{site}.
#'
#' @seealso \code{\link{summarize_hydro_metrics}} for across-year summarization
#'   and ERFA mapping, \code{\link{list_metrics}} for available metrics,
#'   \code{\link{hydro_control}} for expert tuning parameters.
#'
#' @export
#'
#' @examples
#' date <- seq.Date(as.Date("2001-01-01"), as.Date("2005-12-31"), by = "day")
#' Q <- 2 + sin(seq_along(date) * 2 * pi / 365)
#' out <- calculate_hydro_metrics(Q, date, metrics = c("amax1d", "amin1d"))
#' out
calculate_hydro_metrics <- function(
    Q,
    date,
    metrics          = NULL,
    water_year_start = 10L,
    threshold_scope  = c("global", "annual", "seasonal", "baseline"),
    baseline         = NULL,
    high_thresh_quantile = 0.9,
    low_thresh_quantile  = 0.1,
    zero_threshold   = 0.02,
    control          = hydro_control()
) {

  # ---- validate arguments
  threshold_scope <- match.arg(threshold_scope)

  water_year_start <- as.integer(water_year_start)
  if (is.na(water_year_start) || water_year_start < 1L || water_year_start > 12L) {
    stop("water_year_start must be an integer in 1..12.", call. = FALSE)
  }

  if (!inherits(control, "hydro_control")) {
    stop("control must be a hydro_control() object.", call. = FALSE)
  }

  metric_keys <- .flow_resolve_metrics(metrics)
  zero_parsed <- .hydro_parse_zero_threshold(zero_threshold)

  datev <- .hydro_as_date(date, name = "date")
  .hydro_validate_daily_dates(datev)

  # Baseline validation
  baseline_parsed <- list(years = NULL, dates = NULL)
  if (identical(threshold_scope, "baseline")) {
    if (is.null(baseline)) {
      stop("threshold_scope='baseline' requires the baseline argument.", call. = FALSE)
    }
    baseline_parsed <- .hydro_parse_baseline(baseline, datev)
  }

  is_multi <- !is.vector(Q)

  # ---- empty input guard
  if (length(datev) == 0L) {
    empty <- .hydro_empty_annual_result(metric_keys)
    if (is_multi) {
      site_names <- colnames(Q)
      if (is.null(site_names)) site_names <- paste0("site", seq_len(ncol(Q)))
      out_list <- lapply(site_names, function(s) { e <- empty; e$site <- s; e })
      return(dplyr::bind_rows(out_list))
    }
    return(empty)
  }

  # ---- per-site computation
  calc_site <- function(Q_site, site_name = NULL) {
    if (length(Q_site) != length(datev)) {
      stop("Length mismatch: discharge and date must have the same length.", call. = FALSE)
    }
    Q_site <- .hydro_as_numeric(Q_site, name = "Q")

    yr_cal   <- lubridate::year(datev)
    mon_all  <- lubridate::month(datev)
    jday_all <- lubridate::yday(datev)

    # Water year assignment
    if (water_year_start == 1L) {
      yr_all <- yr_cal
    } else {
      yr_all <- yr_cal
      wy_shift <- mon_all >= water_year_start
      yr_all[wy_shift] <- yr_all[wy_shift] + 1L
    }

    # Precompute thresholds
    thr_obj <- .hydro_build_threshold_object(
      Qv = Q_site, datev = datev, threshold_scope = threshold_scope,
      high_q = high_thresh_quantile, low_q = low_thresh_quantile,
      zero_parsed = zero_parsed, min_finite = control$min_finite_thresholds,
      baseline_parsed = baseline_parsed
    )

    reg_all <- .flow_metric_registry()
    reg_use <- reg_all[match(metric_keys, reg_all$metric_key), , drop = FALSE]

    # Split by grain
    annual_keys <- metric_keys[reg_use$grain == "annual"]
    period_keys <- metric_keys[reg_use$grain == "period"]

    result_parts <- list()

    # ---- Annual-grain metrics
    if (length(annual_keys) > 0L) {
      reg_annual <- reg_use[reg_use$grain == "annual", , drop = FALSE]
      years <- sort(unique(yr_all))
      annual_rows <- vector("list", length(years))

      for (i in seq_along(years)) {
        y <- years[i]
        idx <- which(yr_all == y)
        if (length(idx) == 0L) next

        Qy   <- Q_site[idx]
        jdy  <- jday_all[idx]
        mony <- mon_all[idx]

        # Thresholds for this year
        if (identical(thr_obj$scope, "annual")) {
          hl <- .hydro_compute_hilo_thresholds(
            Qy, high_thresh_quantile, low_thresh_quantile,
            control$min_finite_thresholds)
          z <- .hydro_compute_zero_threshold(
            Qy, zero_parsed, control$min_finite_thresholds)
          thr_high_vec <- rep(hl$high_thr, length(Qy))
          thr_low_vec  <- rep(hl$low_thr,  length(Qy))
          thr_zero_vec <- rep(z, length(Qy))
        } else if (identical(thr_obj$scope, "seasonal")) {
          thr_high_vec <- as.numeric(thr_obj$high[as.character(mony)])
          thr_low_vec  <- as.numeric(thr_obj$low[as.character(mony)])
          thr_zero_vec <- as.numeric(thr_obj$zero[as.character(mony)])
        } else {
          thr_high_vec <- rep(thr_obj$high, length(Qy))
          thr_low_vec  <- rep(thr_obj$low,  length(Qy))
          thr_zero_vec <- rep(thr_obj$zero, length(Qy))
        }

        ctx <- .hydro_annual_context(
          Qy = Qy, jdy = jdy, mony = mony, y = y,
          thr_high_vec = thr_high_vec, thr_low_vec = thr_low_vec,
          thr_zero_vec = thr_zero_vec, ctrl = control
        )

        vals <- numeric(length(annual_keys))
        for (k in seq_along(annual_keys)) {
          vals[k] <- reg_annual$compute[[k]](ctx)
        }

        annual_rows[[i]] <- tibble::tibble(
          year = y,
          metric_key = annual_keys,
          group = factor(reg_annual$group,
                         levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
          grain = "annual",
          value = as.numeric(vals)
        )
      }
      result_parts[["annual"]] <- dplyr::bind_rows(annual_rows)
    }

    # ---- Period-grain metrics (computed over entire record)
    if (length(period_keys) > 0L) {
      reg_period <- reg_use[reg_use$grain == "period", , drop = FALSE]
      # Period-grain metrics would get a full-record context here.
      # Currently no period-grain metrics exist; this is the extension point.
      # When implemented, each metric's compute handler would receive a
      # period-level context with the full Q, date, etc.
      vals <- numeric(length(period_keys))
      for (k in seq_along(period_keys)) {
        vals[k] <- NA_real_  # placeholder for future period-grain compute
      }
      result_parts[["period"]] <- tibble::tibble(
        year = NA_integer_,
        metric_key = period_keys,
        group = factor(reg_period$group,
                       levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
        grain = "period",
        value = as.numeric(vals)
      )
    }

    out <- dplyr::bind_rows(result_parts)

    if (!is.null(site_name) && nrow(out) > 0L) {
      out$site <- site_name
      out <- dplyr::relocate(out, site, .before = year)
    }
    out
  }

  # ---- Multi-site dispatch
  if (is_multi) {
    if (!is.matrix(Q) && !is.data.frame(Q)) {
      stop("Q must be a vector, matrix, or data.frame.", call. = FALSE)
    }
    if (nrow(Q) != length(datev)) {
      stop("nrow(Q) must equal length(date).", call. = FALSE)
    }
    site_names <- colnames(Q)
    if (is.null(site_names)) site_names <- paste0("site", seq_len(ncol(Q)))

    out_list <- vector("list", ncol(Q))
    for (i in seq_len(ncol(Q))) {
      out_list[[i]] <- calc_site(Q[, i], site_name = site_names[i])
    }
    return(dplyr::bind_rows(out_list))
  }

  # ---- Single-site
  calc_site(Q, site_name = NULL)
}

#' Calculate flow metrics from daily discharge
#'
#' @description
#' Flow-named API alias for \code{\link{calculate_hydro_metrics}}.
#'
#' @details
#' This function forwards all arguments to
#' \code{\link{calculate_hydro_metrics}} and returns the same structure.
#'
#' @inheritParams calculate_hydro_metrics
#'
#' @return A tibble with columns:
#'   \code{year}, \code{metric_key}, \code{group}, \code{grain}, \code{value},
#'   and optional \code{site} for multi-site input.
#'
#' @examples
#' date <- seq.Date(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
#' Q <- 2 + sin(seq_along(date) * 2 * pi / 365)
#' calculate_flow_metrics(Q, date, metrics = c("amax1d", "amin1d"))
#'
#' @export
calculate_flow_metrics <- function(
    Q,
    date,
    metrics          = NULL,
    water_year_start = 10L,
    threshold_scope  = c("global", "annual", "seasonal", "baseline"),
    baseline         = NULL,
    high_thresh_quantile = 0.9,
    low_thresh_quantile  = 0.1,
    zero_threshold   = 0.02,
    control          = hydro_control()
) {
  calculate_hydro_metrics(
    Q = Q,
    date = date,
    metrics = metrics,
    water_year_start = water_year_start,
    threshold_scope = threshold_scope,
    baseline = baseline,
    high_thresh_quantile = high_thresh_quantile,
    low_thresh_quantile = low_thresh_quantile,
    zero_threshold = zero_threshold,
    control = control
  )
}

# ------------------------------------------------------------------------------
# INTERNAL: empty result for annual-grain metrics
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_empty_annual_result <- function(metric_keys) {
  reg <- .flow_metric_registry()
  reg <- reg[match(metric_keys, reg$metric_key), , drop = FALSE]
  tibble::tibble(
    year = integer(0),
    metric_key = character(0),
    group = factor(character(0), levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
    grain = character(0),
    value = numeric(0)
  )
}

