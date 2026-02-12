# ==============================================================================
# Script: R/hydro_metrics.R
# Functions under test: calculate_hydro_metrics(), .hydro_metric_registry_base(),
# .hydro_validate_registry(), .hydro_metric_sets(), .hydro_registry_erfa70_expanded(),
# .hydro_erfa70_base_order(), .hydro_resolve_metric_keys(), .hydro_resolve_summaries(),
# .hydro_validate_daily_dates(), .hydro_rollmean_right(), .hydro_smooth_center(),
# .hydro_validate_baseline_spec(), .hydro_compute_hilo_thresholds(),
# .hydro_compute_zero_threshold(), .hydro_annual_context(),
# .hydro_build_threshold_object(), .hydro_monthly_means(),
# .hydro_empty_result()
# Purpose: Registry-backed hydrological metric computation and summarization.
# ==============================================================================

# ------------------------------------------------------------------------------
# CENTRAL REGISTRY (HYDROLOGICAL BASE METRICS)
# ------------------------------------------------------------------------------
# NOTE: Keep your corrected .hydro_metric_registry_base() here (the version that
# builds purposes programmatically). Not repeated below to keep patch focused.
# ------------------------------------------------------------------------------

#' Build the base metric registry with group, label, and purpose tags.
#' @return Tibble with metric definitions and purpose list-column.
#' @keywords internal
.hydro_metric_registry_base <- function() {

  reg <- tibble::tibble(
    metric_key = c(
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
    group = c(
      rep("HF", 8),
      rep("MF", 12),
      rep("LF", 8),
      rep("RFC", 4),
      rep("IF", 3)
    ),
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
    )
  )

  # ---- purposes list-column (must be length nrow(reg))
  purposes <- vector("list", nrow(reg))

  # Baseline tag for all current metrics (they are all hydrology + eflow + ERFA)
  for (i in seq_len(nrow(reg))) {
    purposes[[i]] <- c("hydrology", "eflow", "erfa")
  }

  add_tags <- function(keys, tags) {
    ii <- match(keys, reg$metric_key)
    ii <- ii[!is.na(ii)]
    for (j in ii) {
      purposes[[j]] <- unique(c(purposes[[j]], tags))
    }
  }

  # HF: often relevant for navigation/energy operations
  add_tags(c("amax1d","amax3d","amax7d","amax30d","amax90d"), c("navigation","energy"))
  add_tags(c("juldate_amax1d","num_high_pulses","avg_dur_high_pulses"), c("navigation"))

  # Monthly means: broad seasonal relevance
  add_tags(c(
    "january_flow","february_flow","march_flow","april_flow","may_flow","june_flow",
    "july_flow","august_flow","september_flow","october_flow","november_flow","december_flow"
  ), c("irrigation","energy","navigation"))

  # LF: relevant for irrigation/navigation/energy constraints
  add_tags(c("amin1d","amin3d","amin7d","amin30d","amin90d"), c("irrigation","navigation","energy"))
  add_tags(c("juldate_amin1d","num_low_pulses","avg_dur_low_pulses"), c("irrigation","navigation"))

  # Rate-of-change: ramping (energy) and operational stability (navigation)
  add_tags(c("mean_rate_rise","mean_rate_fall","num_rises","num_falls"), c("energy","navigation"))

  # Intermittency: relevant broadly
  add_tags(c("num_zero_seq","avg_dur_zero_seq","time_main_zero_seq"), c("irrigation","energy","navigation"))

  reg$purposes <- purposes

  compute <- rep(list(NULL), nrow(reg))
  requires <- rep(list(character(0)), nrow(reg))

  set_compute <- function(keys, fn, req) {
    ii <- match(keys, reg$metric_key)
    ii <- ii[!is.na(ii)]
    for (j in ii) {
      compute[[j]] <<- fn
      requires[[j]] <<- req
    }
  }

  set_compute("amax1d", function(ctx) .hydro_safe_max(ctx$Qy), character(0))
  set_compute("amax3d", function(ctx) .hydro_safe_max(ctx$get("r3")), c("r3"))
  set_compute("amax7d", function(ctx) .hydro_safe_max(ctx$get("r7")), c("r7"))
  set_compute("amax30d", function(ctx) .hydro_safe_max(ctx$get("r30")), c("r30"))
  set_compute("amax90d", function(ctx) .hydro_safe_max(ctx$get("r90")), c("r90"))
  set_compute("juldate_amax1d", function(ctx) {
    imax <- .hydro_safe_which_max(ctx$Qy)
    if (is.na(imax)) NA_real_ else ctx$jdy[imax]
  }, character(0))
  set_compute("num_high_pulses", function(ctx) ctx$get("hp")$n_runs, c("hp"))
  set_compute("avg_dur_high_pulses", function(ctx) ctx$get("hp")$mean_len, c("hp"))

  set_compute("january_flow", function(ctx) ctx$get("mm")$mean[1], c("mm"))
  set_compute("february_flow", function(ctx) ctx$get("mm")$mean[2], c("mm"))
  set_compute("march_flow", function(ctx) ctx$get("mm")$mean[3], c("mm"))
  set_compute("april_flow", function(ctx) ctx$get("mm")$mean[4], c("mm"))
  set_compute("may_flow", function(ctx) ctx$get("mm")$mean[5], c("mm"))
  set_compute("june_flow", function(ctx) ctx$get("mm")$mean[6], c("mm"))
  set_compute("july_flow", function(ctx) ctx$get("mm")$mean[7], c("mm"))
  set_compute("august_flow", function(ctx) ctx$get("mm")$mean[8], c("mm"))
  set_compute("september_flow", function(ctx) ctx$get("mm")$mean[9], c("mm"))
  set_compute("october_flow", function(ctx) ctx$get("mm")$mean[10], c("mm"))
  set_compute("november_flow", function(ctx) ctx$get("mm")$mean[11], c("mm"))
  set_compute("december_flow", function(ctx) ctx$get("mm")$mean[12], c("mm"))

  set_compute("amin1d", function(ctx) .hydro_safe_min(ctx$Qy), character(0))
  set_compute("amin3d", function(ctx) .hydro_safe_min(ctx$get("r3")), c("r3"))
  set_compute("amin7d", function(ctx) .hydro_safe_min(ctx$get("r7")), c("r7"))
  set_compute("amin30d", function(ctx) .hydro_safe_min(ctx$get("r30")), c("r30"))
  set_compute("amin90d", function(ctx) .hydro_safe_min(ctx$get("r90")), c("r90"))
  set_compute("juldate_amin1d", function(ctx) {
    imin <- .hydro_safe_which_min(ctx$Qy)
    if (is.na(imin)) NA_real_ else ctx$jdy[imin]
  }, character(0))
  set_compute("num_low_pulses", function(ctx) ctx$get("lp")$n_runs, c("lp"))
  set_compute("avg_dur_low_pulses", function(ctx) ctx$get("lp")$mean_len, c("lp"))

  set_compute("mean_rate_rise", function(ctx) .hydro_safe_mean(ctx$get("roc")$rise), c("roc"))
  set_compute("mean_rate_fall", function(ctx) .hydro_safe_mean(ctx$get("roc")$fall), c("roc"))
  set_compute("num_rises", function(ctx) ctx$get("roc")$n_rise, c("roc"))
  set_compute("num_falls", function(ctx) ctx$get("roc")$n_fall, c("roc"))

  set_compute("num_zero_seq", function(ctx) ctx$get("zp")$n_runs, c("zp"))
  set_compute("avg_dur_zero_seq", function(ctx) ctx$get("zp")$mean_len, c("zp"))
  set_compute("time_main_zero_seq", function(ctx) {
    idx <- ctx$get("zp")$start_idx_longest
    if (is.na(idx)) NA_real_ else ctx$jdy[idx]
  }, c("zp"))

  reg$compute <- compute
  reg$requires <- requires
  reg
}

#' Validate registry structure, metric sets, and ERFA mappings.
#' @param strict Logical; if TRUE, validation warnings are treated as errors.
#' @return Invisible TRUE when validation passes.
#' @keywords internal
.hydro_validate_registry <- function(strict = FALSE) {

  .fail <- function(...) stop(..., call. = FALSE)
  .warn <- function(...) {
    msg <- paste0(...)
    if (isTRUE(strict)) .fail(msg)
    warning(msg, call. = FALSE)
  }

  reg <- .hydro_metric_registry_base()

  # ---- Structure checks
  required_cols <- c("metric_key", "group", "label", "purposes", "compute", "requires")
  missing_cols <- setdiff(required_cols, names(reg))
  if (length(missing_cols) > 0L) {
    .fail("Registry missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.character(reg$metric_key) || anyNA(reg$metric_key) || any(reg$metric_key == "")) {
    .fail("Registry metric_key must be non-empty character values without NA.")
  }

  # Unique metric keys
  if (anyDuplicated(reg$metric_key) > 0L) {
    dups <- unique(reg$metric_key[duplicated(reg$metric_key)])
    .fail("Duplicate metric_key values in registry: ", paste(dups, collapse = ", "))
  }

  # Group sanity (allow NA for future non-eflow metrics, but warn)
  allowed_groups <- c("HF", "MF", "LF", "RFC", "IF")
  bad_group <- !is.na(reg$group) & !reg$group %in% allowed_groups
  if (any(bad_group)) {
    .fail(
      "Registry contains unknown group values: ",
      paste(unique(reg$group[bad_group]), collapse = ", "),
      ". Allowed: ", paste(allowed_groups, collapse = ", "), " (or NA)."
    )
  }
  if (any(is.na(reg$group))) {
    .warn("Registry contains metrics with group = NA. This is allowed but ERFA-style grouping will not apply.")
  }

  # Purposes list-column checks
  if (!is.list(reg$purposes)) {
    .fail("Registry purposes must be a list-column of character vectors.")
  }

  bad_purpose <- vapply(reg$purposes, function(x) {
    !(is.character(x) && length(x) >= 1L && all(!is.na(x)) && all(nzchar(x)))
  }, logical(1))

  if (any(bad_purpose)) {
    idx <- which(bad_purpose)
    .fail("Registry purposes must be non-empty character vectors (no NA/empty). Bad rows: ", paste(idx, collapse = ", "))
  }

  # Normalize to unique purposes per metric (warn if duplicates)
  has_dup_purpose <- vapply(reg$purposes, function(x) length(unique(x)) != length(x), logical(1))
  if (any(has_dup_purpose)) {
    keys <- reg$metric_key[has_dup_purpose]
    .warn("Some metrics have duplicated purpose tags; consider de-duplicating: ", paste(keys, collapse = ", "))
  }

  if (!is.list(reg$compute) || length(reg$compute) != nrow(reg)) {
    .fail("Registry compute must be a list-column aligned to registry rows.")
  }
  bad_compute <- vapply(reg$compute, function(x) !is.null(x) && !is.function(x), logical(1))
  if (any(bad_compute)) {
    keys <- reg$metric_key[bad_compute]
    .fail("Registry compute entries must be functions or NULL. Bad rows: ", paste(keys, collapse = ", "))
  }

  if (!is.list(reg$requires) || length(reg$requires) != nrow(reg)) {
    .fail("Registry requires must be a list-column aligned to registry rows.")
  }
  bad_requires <- vapply(reg$requires, function(x) {
    !(is.character(x) && all(!is.na(x)) && all(nzchar(x)))
  }, logical(1))
  if (any(bad_requires)) {
    keys <- reg$metric_key[bad_requires]
    .fail("Registry requires entries must be character vectors (no NA/empty). Bad rows: ", paste(keys, collapse = ", "))
  }

  # ---- Metric sets checks
  sets <- .hydro_metric_sets()
  if (!is.list(sets) || length(sets) == 0L || is.null(names(sets))) {
    .fail(".hydro_metric_sets() must return a named list of character vectors.")
  }

  for (nm in names(sets)) {
    s <- sets[[nm]]
    if (!is.character(s) || length(s) == 0L || anyNA(s) || any(s == "")) {
      .fail("Metric set '", nm, "' must be a non-empty character vector without NA/empty strings.")
    }
    unknown <- setdiff(s, reg$metric_key)
    if (length(unknown) > 0L) {
      .fail("Metric set '", nm, "' contains unknown metric_key values: ", paste(unknown, collapse = ", "))
    }
    if (anyDuplicated(s) > 0L) {
      dups <- unique(s[duplicated(s)])
      .fail("Metric set '", nm, "' contains duplicate metric_key values: ", paste(dups, collapse = ", "))
    }
  }

  for (nm in names(sets)) {
    s <- sets[[nm]]
    idx <- match(s, reg$metric_key)
    compute_ok <- vapply(reg$compute[idx], is.function, logical(1))
    if (any(!compute_ok)) {
      missing_compute <- s[!compute_ok]
      .fail(
        "Metric set '", nm, "' contains metrics without compute functions: ",
        paste(missing_compute, collapse = ", ")
      )
    }
  }

  # ---- ERFA70 expanded mapping checks (only if set exists)
  if ("erfa70" %in% names(sets)) {
    if (!identical(sets[["erfa70"]], .hydro_erfa70_base_order())) {
      .fail("ERFA70 metric set ordering must match .hydro_erfa70_base_order().")
    }

    erfa70_exp <- .hydro_registry_erfa70_expanded()

    req_erfa_cols <- c("metric_id", "group", "metric_key", "stat", "metric")
    miss_erfa_cols <- setdiff(req_erfa_cols, names(erfa70_exp))
    if (length(miss_erfa_cols) > 0L) {
      .fail("ERFA70 expanded registry missing columns: ", paste(miss_erfa_cols, collapse = ", "))
    }

    # Expect metric_id to be 1..70 and unique
    if (!is.integer(erfa70_exp$metric_id)) {
      erfa70_exp$metric_id <- as.integer(erfa70_exp$metric_id)
    }
    if (anyNA(erfa70_exp$metric_id) || anyDuplicated(erfa70_exp$metric_id) > 0L) {
      .fail("ERFA70 metric_id must be unique and non-NA.")
    }
    if (!identical(sort(erfa70_exp$metric_id), seq_len(nrow(erfa70_exp)))) {
      .warn("ERFA70 metric_id is not a contiguous 1..N sequence. If intentional, ignore this warning.")
    }

    # ERFA70 should be base metrics x {med, iqr}
    base_keys <- sets[["erfa70"]]
    stats_expected <- c("med", "iqr")
    n_expected <- length(base_keys) * length(stats_expected)

    if (nrow(erfa70_exp) != n_expected) {
      .fail(
        "ERFA70 expanded registry row count mismatch: expected ", n_expected,
        " (", length(base_keys), " base metrics x 2 stats), got ", nrow(erfa70_exp), "."
      )
    }

    if (!all(erfa70_exp$stat %in% stats_expected)) {
      .fail("ERFA70 expanded registry stat must be 'med' or 'iqr' only.")
    }

    unknown_erfa_keys <- setdiff(erfa70_exp$metric_key, reg$metric_key)
    if (length(unknown_erfa_keys) > 0L) {
      .fail("ERFA70 expanded registry contains unknown metric_key values: ", paste(unknown_erfa_keys, collapse = ", "))
    }

    # Metric name consistency: *_m for med, *_v for iqr
    suffix_ok <- (erfa70_exp$stat == "med" & grepl("_m$", erfa70_exp$metric)) |
      (erfa70_exp$stat == "iqr" & grepl("_v$", erfa70_exp$metric))

    if (!all(suffix_ok)) {
      bad <- erfa70_exp[!suffix_ok, c("metric_key", "stat", "metric")]
      .fail("ERFA70 expanded registry has inconsistent metric suffixes for med/iqr.")
    }
  }

  invisible(TRUE)
}
# ------------------------------------------------------------------------------
# METRIC SETS (ORDERED SUBSETS)
# ------------------------------------------------------------------------------
#' Return the named list of metric sets used for selection.
#' @return Named list of character vectors of metric keys.
#' @keywords internal
.hydro_metric_sets <- function() {
  list(
    erfa70 = .hydro_erfa70_base_order()
  )
}

# ------------------------------------------------------------------------------
# ERFA70 EXPANDED REGISTRY (LEGACY 70 metric_id mapping)
# ------------------------------------------------------------------------------
#' Expand the ERFA70 registry to include med/iqr legacy metric names.
#' @return Tibble mapping metric_id, metric_key, stat, and group.
#' @keywords internal
.hydro_registry_erfa70_expanded <- function() {

  order_base <- .hydro_erfa70_base_order()

  base_meta <- .hydro_metric_registry_base()
  base_meta <- base_meta[match(order_base, base_meta$metric_key), , drop = FALSE]
  if (anyNA(base_meta$metric_key)) {
    stop("ERFA70 expanded registry references unknown base metric_key values.", call. = FALSE)
  }

  stat_vec <- rep(c("med", "iqr"), times = nrow(base_meta))
  metric_key_vec <- rep(base_meta$metric_key, each = 2)
  group_vec <- rep(base_meta$group, each = 2)

  suffix <- ifelse(stat_vec == "med", "m", "v")
  legacy_metric <- paste0(metric_key_vec, "_", suffix)

  tibble::tibble(
    metric_id = seq_len(length(metric_key_vec)),
    group = factor(group_vec, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
    metric_key = metric_key_vec,
    stat = stat_vec,
    metric = legacy_metric
  )
}


# ------------------------------------------------------------------------------
# INTERNAL: Canonical ERFA base metric order (35 base metrics)
# ------------------------------------------------------------------------------
#' Return the canonical ERFA70 base metric order.
#' @return Character vector of metric keys.
#' @keywords internal
.hydro_erfa70_base_order <- function() {
  c(
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
}


# ------------------------------------------------------------------------------
# METRIC SELECTION RESOLVER
# ------------------------------------------------------------------------------
#' Resolve requested metric keys from a set, explicit list, or purpose tag.
#' @param metric_set Character scalar name of a registered metric set.
#' @param metrics Optional character vector of metric keys to use directly.
#' @param purpose Optional character scalar purpose tag for selection.
#' @return Character vector of metric keys to compute.
#' @keywords internal
.hydro_resolve_metric_keys <- function(metric_set = "erfa70", metrics = NULL, purpose = NULL) {
  reg <- .hydro_metric_registry_base()
  all_keys <- reg$metric_key

  if (!is.null(metrics)) {
    metrics <- as.character(metrics)
    unknown <- setdiff(metrics, all_keys)
    if (length(unknown) > 0L) {
      stop("Unknown metrics: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    return(metrics)
  }

  if (!is.null(metric_set)) {
    sets <- .hydro_metric_sets()
    if (!metric_set %in% names(sets)) {
      stop("Unknown metric_set: ", metric_set, call. = FALSE)
    }
    keys <- sets[[metric_set]]
    unknown <- setdiff(keys, all_keys)
    if (length(unknown) > 0L) {
      stop("metric_set contains unknown metrics: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    return(keys)
  }

  if (!is.null(purpose)) {
    purpose <- as.character(purpose)[1]
    keep <- vapply(reg$purposes, function(x) purpose %in% x, logical(1))
    keys <- reg$metric_key[keep]
    if (length(keys) == 0L) {
      stop("No metrics found for purpose: ", purpose, call. = FALSE)
    }
    return(keys)
  }

  .hydro_metric_sets()[["erfa70"]]
}

# ------------------------------------------------------------------------------
# SUMMARY FUNCTIONS RESOLVER
# ------------------------------------------------------------------------------
#' Resolve summary statistic functions from labels or a named list.
#' @param summaries Character vector of summary names or a named list of functions.
#' @return Named list of summary functions.
#' @keywords internal
.hydro_resolve_summaries <- function(summaries = c("med", "iqr")) {
  if (is.character(summaries)) {
    out <- list()
    for (nm in summaries) {
      if (identical(nm, "med")) {
        out[["med"]] <- function(x) stats::median(x, na.rm = TRUE)
      } else if (identical(nm, "iqr")) {
        out[["iqr"]] <- function(x) stats::IQR(x, na.rm = TRUE)
      } else {
        stop("Unknown summary: ", nm, call. = FALSE)
      }
    }
    return(out)
  }

  if (is.list(summaries) && length(summaries) > 0L && !is.null(names(summaries))) {
    return(summaries)
  }

  stop("summaries must be a character vector (e.g., c('med','iqr')) or a named list of functions.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# INTERNAL: Date-axis validation
# ------------------------------------------------------------------------------
#' Validate daily date ordering, uniqueness, and regular spacing.
#' @param datev Date vector to validate.
#' @return Invisible TRUE when validation passes.
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
      "date has gaps (diff != 1 day). Metrics assume regular daily spacing; ",
      "rolling means and rise/fall counts may be biased.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# INTERNAL: rolling mean within-year (prevents year leakage)
# ------------------------------------------------------------------------------
#' Compute a right-aligned rolling mean with NA padding.
#' @param x Numeric vector to smooth.
#' @param k Integer window length.
#' @return Numeric vector of rolling means.
#' @keywords internal
.hydro_rollmean_right <- function(x, k) {
  k <- as.integer(k)
  if (length(x) == 0L) return(numeric(0))
  if (k <= 1L) return(as.numeric(x))
  zoo::rollmean(x, k, align = "right", fill = NA)
}

# ------------------------------------------------------------------------------
# INTERNAL: centered smoothing within-year (for ROC only)
# ------------------------------------------------------------------------------
#' Compute a centered rolling mean with NA padding.
#' @param x Numeric vector to smooth.
#' @param k Integer window length.
#' @return Numeric vector of rolling means.
#' @keywords internal
.hydro_smooth_center <- function(x, k) {
  k <- as.integer(k)
  if (length(x) == 0L) return(numeric(0))
  if (k <= 1L) return(as.numeric(x))
  zoo::rollmean(x, k, align = "center", fill = NA)
}

# ------------------------------------------------------------------------------
# INTERNAL: annual context with lazy caching
# ------------------------------------------------------------------------------
#' Build a lazy annual context for metric computation within a single year.
#' @param Qy Numeric vector of discharge values for one year.
#' @param jdy Integer Julian day vector aligned to Qy.
#' @param mony Integer month vector aligned to Qy.
#' @param y Integer year label.
#' @param thr_high_vec Numeric vector of high thresholds aligned to Qy.
#' @param thr_low_vec Numeric vector of low thresholds aligned to Qy.
#' @param thr_zero_vec Numeric vector of zero thresholds aligned to Qy.
#' @param roc_eps Numeric scalar deadband for rate-of-change metrics.
#' @param roc_smooth_k Integer window length for ROC smoothing.
#' @param month_min_frac Numeric scalar in \code{[0,1]} for monthly completeness.
#' @param report_coverage Logical; if TRUE, cache monthly coverage fractions.
#' @return List with raw inputs and ctx$get() accessor for cached values.
#' @keywords internal
.hydro_annual_context <- function(
    Qy, jdy, mony, y,
    thr_high_vec, thr_low_vec, thr_zero_vec,
    roc_eps, roc_smooth_k,
    month_min_frac, report_coverage
) {
  cache <- new.env(parent = emptyenv())

  get <- function(name) {
    if (exists(name, envir = cache, inherits = FALSE)) {
      return(base::get(name, envir = cache, inherits = FALSE))
    }

    if (identical(name, "r3")) {
      val <- .hydro_rollmean_right(Qy, 3L)
    } else if (identical(name, "r7")) {
      val <- .hydro_rollmean_right(Qy, 7L)
    } else if (identical(name, "r30")) {
      val <- .hydro_rollmean_right(Qy, 30L)
    } else if (identical(name, "r90")) {
      val <- .hydro_rollmean_right(Qy, 90L)
    } else if (identical(name, "hp")) {
      val <- .hydro_seq_stats(Qy > thr_high_vec)
    } else if (identical(name, "lp")) {
      val <- .hydro_seq_stats(Qy < thr_low_vec)
    } else if (identical(name, "zp")) {
      val <- .hydro_seq_stats(Qy <= thr_zero_vec)
    } else if (identical(name, "mm")) {
      val <- .hydro_monthly_means(Qy, y = y, mony = mony, month_min_frac = month_min_frac, report_coverage = report_coverage)
    } else if (identical(name, "roc")) {
      Qroc <- .hydro_smooth_center(Qy, roc_smooth_k)
      dQy <- diff(Qroc)
      rise_vals <- dQy[dQy > roc_eps]
      fall_vals <- abs(dQy[dQy < -roc_eps])
      val <- list(
        dQ = dQy,
        rise = rise_vals,
        fall = fall_vals,
        n_rise = sum(dQy > roc_eps, na.rm = TRUE),
        n_fall = sum(dQy < -roc_eps, na.rm = TRUE)
      )
    } else {
      stop("Unknown context key: ", name, call. = FALSE)
    }

    assign(name, val, envir = cache)
    val
  }

  list(
    Qy = Qy,
    jdy = jdy,
    mony = mony,
    y = y,
    thr_high_vec = thr_high_vec,
    thr_low_vec = thr_low_vec,
    thr_zero_vec = thr_zero_vec,
    roc_eps = roc_eps,
    roc_smooth_k = roc_smooth_k,
    month_min_frac = month_min_frac,
    report_coverage = report_coverage,
    get = get
  )
}

# ------------------------------------------------------------------------------
# INTERNAL: validate threshold baseline specification
# ------------------------------------------------------------------------------
#' Validate baseline selection inputs for threshold computation.
#' @param threshold_scope Character scalar threshold scope.
#' @param baseline_years Optional integer vector of baseline years.
#' @param baseline_dates Optional length-2 Date vector of baseline bounds.
#' @param datev Date vector for overlap checks.
#' @return Invisible TRUE when validation passes.
#' @keywords internal
.hydro_validate_baseline_spec <- function(threshold_scope, baseline_years, baseline_dates, datev) {
  if (!identical(threshold_scope, "baseline")) return(invisible(TRUE))

  if (is.null(baseline_years) && is.null(baseline_dates)) {
    stop("threshold_scope='baseline' requires baseline_years or baseline_dates.", call. = FALSE)
  }

  if (!is.null(baseline_dates)) {
    if (!inherits(baseline_dates, "Date")) baseline_dates <- as.Date(baseline_dates)
    if (length(baseline_dates) != 2L || anyNA(baseline_dates)) {
      stop("baseline_dates must be a length-2 Date vector: c(start, end).", call. = FALSE)
    }
    if (baseline_dates[1] > baseline_dates[2]) {
      stop("baseline_dates must be in increasing order: c(start, end).", call. = FALSE)
    }
    if (baseline_dates[2] < min(datev) || baseline_dates[1] > max(datev)) {
      stop("baseline_dates do not overlap the provided date range.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# INTERNAL: compute high/low thresholds for a subset, with finite-value guard
# ------------------------------------------------------------------------------
#' Compute high and low flow thresholds from a finite subset.
#' @param Qv Numeric vector of discharge values.
#' @param high_q Numeric scalar quantile for high threshold.
#' @param low_q Numeric scalar quantile for low threshold.
#' @param min_finite Integer minimum number of finite values required.
#' @return List with high_thr and low_thr numeric scalars.
#' @keywords internal
.hydro_compute_hilo_thresholds <- function(Qv, high_q, low_q, min_finite) {
  q <- Qv[is.finite(Qv)]
  if (length(q) < min_finite) {
    stop(
      "Insufficient finite discharge values (", length(q), ") to compute thresholds; ",
      "need at least ", min_finite, ".",
      call. = FALSE
    )
  }
  high_thr <- stats::quantile(q, probs = high_q, na.rm = TRUE, names = FALSE)
  low_thr  <- stats::quantile(q, probs = low_q,  na.rm = TRUE, names = FALSE)

  if (!is.finite(high_thr) || !is.finite(low_thr)) {
    stop("Computed high/low thresholds are not finite; check discharge values.", call. = FALSE)
  }

  list(high_thr = high_thr, low_thr = low_thr)
}

# ------------------------------------------------------------------------------
# INTERNAL: compute zero threshold for a subset, allowing relative modes
# ------------------------------------------------------------------------------
#' Compute a zero-flow threshold from absolute or relative specifications.
#' @param Qv Numeric vector of discharge values.
#' @param zero_thr_mode Character scalar; threshold mode name.
#' @param zero_thr Numeric scalar for absolute mode.
#' @param zero_q Numeric scalar quantile for quantile mode.
#' @param zero_frac_median Numeric scalar fraction of median for fraction mode.
#' @param min_finite Integer minimum number of finite values required.
#' @return Numeric scalar zero-flow threshold.
#' @keywords internal
.hydro_compute_zero_threshold <- function(Qv, zero_thr_mode, zero_thr, zero_q, zero_frac_median, min_finite) {
  if (identical(zero_thr_mode, "absolute")) {
    if (!is.numeric(zero_thr) || length(zero_thr) != 1L || !is.finite(zero_thr)) {
      stop("zero_thr must be a finite numeric scalar for zero_thr_mode='absolute'.", call. = FALSE)
    }
    return(zero_thr)
  }

  q <- Qv[is.finite(Qv)]
  if (length(q) < min_finite) {
    stop(
      "Insufficient finite discharge values (", length(q), ") to compute zero threshold; ",
      "need at least ", min_finite, ".",
      call. = FALSE
    )
  }

  if (identical(zero_thr_mode, "quantile")) {
    if (!is.numeric(zero_q) || length(zero_q) != 1L || is.na(zero_q) || zero_q < 0 || zero_q > 1) {
      stop("zero_q must be a numeric scalar in [0,1] for zero_thr_mode='quantile'.", call. = FALSE)
    }
    z <- stats::quantile(q, probs = zero_q, na.rm = TRUE, names = FALSE)
    if (!is.finite(z)) stop("Computed zero threshold is not finite.", call. = FALSE)
    return(z)
  }

  if (identical(zero_thr_mode, "fraction_median")) {
    if (!is.numeric(zero_frac_median) || length(zero_frac_median) != 1L ||
        is.na(zero_frac_median) || zero_frac_median < 0) {
      stop("zero_frac_median must be a numeric scalar >= 0 for zero_thr_mode='fraction_median'.", call. = FALSE)
    }
    med <- stats::median(q, na.rm = TRUE)
    z <- zero_frac_median * med
    if (!is.finite(z)) stop("Computed zero threshold is not finite.", call. = FALSE)
    return(z)
  }

  stop("Unknown zero_thr_mode: ", zero_thr_mode, call. = FALSE)
}

# ------------------------------------------------------------------------------
# INTERNAL: build threshold object based on scope
# Returns a list with:
#   scope
#   global: list(high, low, zero) OR
#   seasonal: named vectors length 12 for each of high/low/zero OR
#   baseline: same as global but estimated on baseline subset
#
# Annual scope is handled inside the year loop.
# ------------------------------------------------------------------------------
#' Build a threshold object for the selected threshold scope.
#' @param Qv Numeric vector of discharge values.
#' @param datev Date vector aligned to Qv.
#' @param threshold_scope Character scalar threshold scope.
#' @param high_q Numeric scalar quantile for high threshold.
#' @param low_q Numeric scalar quantile for low threshold.
#' @param zero_thr_mode Character scalar; threshold mode name.
#' @param zero_thr Numeric scalar for absolute mode.
#' @param zero_q Numeric scalar quantile for quantile mode.
#' @param zero_frac_median Numeric scalar fraction of median for fraction mode.
#' @param min_finite_thresholds Integer minimum number of finite values required.
#' @param baseline_years Optional integer vector of baseline years.
#' @param baseline_dates Optional length-2 Date vector of baseline bounds.
#' @return List describing threshold scope and values.
#' @keywords internal
.hydro_build_threshold_object <- function(
    Qv, datev, threshold_scope,
    high_q, low_q,
    zero_thr_mode, zero_thr, zero_q, zero_frac_median,
    min_finite_thresholds,
    baseline_years = NULL, baseline_dates = NULL
) {

  if (identical(threshold_scope, "annual")) {
    return(list(scope = "annual"))
  }

  if (identical(threshold_scope, "global")) {
    hl <- .hydro_compute_hilo_thresholds(Qv, high_q, low_q, min_finite_thresholds)
    z  <- .hydro_compute_zero_threshold(Qv, zero_thr_mode, zero_thr, zero_q, zero_frac_median, min_finite_thresholds)
    return(list(scope = "global", high = hl$high_thr, low = hl$low_thr, zero = z))
  }

  if (identical(threshold_scope, "baseline")) {
    yr <- lubridate::year(datev)
    keep <- rep(TRUE, length(datev))

    if (!is.null(baseline_years)) {
      keep <- keep & (yr %in% baseline_years)
    }
    if (!is.null(baseline_dates)) {
      bd <- as.Date(baseline_dates)
      keep <- keep & (datev >= bd[1] & datev <= bd[2])
    }

    qsub <- Qv[keep]
    hl <- .hydro_compute_hilo_thresholds(qsub, high_q, low_q, min_finite_thresholds)
    z  <- .hydro_compute_zero_threshold(qsub, zero_thr_mode, zero_thr, zero_q, zero_frac_median, min_finite_thresholds)
    return(list(scope = "baseline", high = hl$high_thr, low = hl$low_thr, zero = z))
  }

  if (identical(threshold_scope, "seasonal")) {
    mon <- lubridate::month(datev)
    high <- rep(NA_real_, 12L)
    low  <- rep(NA_real_, 12L)
    zero <- rep(NA_real_, 12L)

    for (m in 1:12) {
      idx <- which(mon == m)
      qsub <- Qv[idx]
      hl <- .hydro_compute_hilo_thresholds(qsub, high_q, low_q, min_finite_thresholds)
      z  <- .hydro_compute_zero_threshold(qsub, zero_thr_mode, zero_thr, zero_q, zero_frac_median, min_finite_thresholds)
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
# INTERNAL: monthly mean with completeness enforcement and coverage reporting
# ------------------------------------------------------------------------------
#' Compute monthly means with completeness checks and optional coverage fractions.
#' @param Qy Numeric vector of discharge values for a single year.
#' @param y Integer year for month length evaluation.
#' @param mony Integer month vector aligned to Qy.
#' @param month_min_frac Numeric scalar in \code{[0,1]} for minimum monthly completeness.
#' @param report_coverage Logical; if TRUE, return monthly coverage fractions.
#' @return List with mean and optional frac elements.
#' @keywords internal
.hydro_monthly_means <- function(Qy, y, mony, month_min_frac = 0, report_coverage = FALSE) {
  m_mean <- rep(NA_real_, 12L)
  m_frac <- rep(NA_real_, 12L)

  for (m in 1:12) {
    idx <- which(mony == m)
    if (length(idx) == 0L) {
      m_mean[m] <- NA_real_
      m_frac[m] <- 0
      next
    }

    # expected days in that calendar month
    first_day <- as.Date(sprintf("%04d-%02d-01", y, m))
    expected <- as.integer(lubridate::days_in_month(first_day))

    qsub <- Qy[idx]
    obs <- sum(is.finite(qsub))

    frac <- if (expected > 0L) obs / expected else NA_real_
    m_frac[m] <- frac

    if (!is.finite(frac) || frac < month_min_frac) {
      m_mean[m] <- NA_real_
    } else {
      v <- mean(qsub, na.rm = TRUE)
      m_mean[m] <- if (is.nan(v)) NA_real_ else v
    }
  }

  out <- list(mean = m_mean)
  if (isTRUE(report_coverage)) out$frac = m_frac
  out
}

# ------------------------------------------------------------------------------
# INTERNAL: Empty output builder
# ------------------------------------------------------------------------------
#' Build an empty result table for requested metrics and summaries.
#' @param metric_keys Character vector of metric keys to include.
#' @param site Optional character site name.
#' @param expanded Logical; if TRUE, include stat and metric columns.
#' @param summaries Character vector of summary names for expanded output.
#' @return Tibble with required columns and NA values.
#' @keywords internal
.hydro_empty_result <- function(metric_keys, site = NULL, expanded = TRUE, summaries = c("med", "iqr")) {
  reg <- .hydro_metric_registry_base()
  reg <- reg[match(metric_keys, reg$metric_key), , drop = FALSE]

  if (expanded) {
    summary_fns <- .hydro_resolve_summaries(summaries)
    stat_names <- names(summary_fns)

    out_list <- vector("list", length(stat_names))
    for (i in seq_along(stat_names)) {
      st <- stat_names[i]
      suffix <- if (identical(st, "med")) "m" else if (identical(st, "iqr")) "v" else st
      out_list[[i]] <- tibble::tibble(
        metric_key = reg$metric_key,
        group = factor(reg$group, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
        stat = st,
        metric = paste0(reg$metric_key, "_", suffix),
        value = NA_real_
      )
    }
    out <- dplyr::bind_rows(out_list)
  } else {
    out <- tibble::tibble(
      metric_key = reg$metric_key,
      group = factor(reg$group, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
      value = NA_real_
    )
  }

  if (!is.null(site)) {
    out$site <- site
    out <- dplyr::relocate(out, site, .before = metric_key)
  }

  out
}





# ------------------------------------------------------------------------------
#' Calculate Hydrological Metrics from Daily Discharge
#'
#' @description
#' Calculates registry-defined hydrological metrics from daily discharge and summarizes
#' annual values across years for one or more sites.
#'
#' @details
#' Inputs must represent daily values: \code{date} must be strictly increasing and
#' unique, and its length must match rows of \code{Q}. Gaps in \code{date} are allowed
#' but trigger a warning; rolling means and rise/fall counts assume regular daily
#' spacing and may be biased if gaps are present.
#'
#' Thresholds for pulse and zero-flow metrics are estimated from either the entire
#' series ("global"), each year separately ("annual"), by month across years
#' ("seasonal"), or from a baseline subset ("baseline"). Threshold estimation
#' requires at least \code{min_finite_thresholds} finite values; otherwise an error
#' is thrown. Zero-flow thresholds can be absolute, a quantile of the subset, or a
#' fraction of the subset median.
#'
#' Metric selection uses \code{metrics} directly when provided; otherwise it uses
#' \code{metric_set} (default "erfa70"), or \code{purpose} when \code{metric_set = NULL}.
#' Annual values are summarized with the functions named in \code{summaries}; by
#' default median and IQR. ERFA \code{metric_id} values are populated only when the
#' full ERFA70 metric set and \code{c("med","iqr")} summaries are used.
#'
#' @param Q Numeric vector (single site) or matrix/data.frame (multi-site) of daily
#'   discharge values. For multi-site inputs, columns are sites and optional column
#'   names are used as site identifiers.
#' @param date Date vector corresponding to rows of \code{Q}.
#' @param high_thresh_quantile Numeric scalar in \code{[0,1]} for the high-flow
#'   pulse threshold quantile.
#' @param low_thresh_quantile Numeric scalar in \code{[0,1]} for the low-flow
#'   pulse threshold quantile.
#' @param threshold_scope Character scalar, one of "global", "annual", "seasonal",
#'   "baseline". Controls how thresholds are estimated for high/low pulses and
#'   zero-flow sequences.
#' @param baseline_years Optional integer vector of years used when
#'   \code{threshold_scope = "baseline"}.
#' @param baseline_dates Optional length-2 Date vector \code{c(start, end)} used when
#'   \code{threshold_scope = "baseline"} (inclusive bounds).
#' @param min_finite_thresholds Integer scalar >= 1. Minimum number of finite values
#'   required to estimate thresholds within each subset.
#' @param zero_thr_mode Character scalar, one of "absolute", "quantile",
#'   "fraction_median".
#' @param zero_thr Numeric scalar used when \code{zero_thr_mode = "absolute"}.
#' @param zero_q Numeric scalar in \code{[0,1]} used when \code{zero_thr_mode = "quantile"}.
#' @param zero_frac_median Numeric scalar >= 0 used when \code{zero_thr_mode =
#'   "fraction_median"}; multiplied by the subset median discharge.
#' @param roc_eps Numeric scalar >= 0. Deadband for rise/fall classification; daily
#'   changes with absolute magnitude \code{<= roc_eps} are ignored.
#' @param roc_smooth_k Integer scalar >= 1. Centered moving-average window (days)
#'   applied before rate-of-change calculations; \code{1} disables smoothing.
#' @param month_min_frac Numeric scalar in \code{[0,1]}. Minimum fraction of calendar
#'   days in a month that must have finite discharge values to compute that monthly
#'   mean; otherwise NA. Default 0 disables completeness filtering.
#' @param report_coverage Logical. If TRUE, compute monthly coverage fractions for
#'   internal context (not returned).
#' @param metric_set Character scalar. Name of a registered metric set (default
#'   "erfa70"). Use \code{NULL} to select metrics via \code{purpose}.
#' @param metrics Optional character vector of base metric keys to compute; overrides
#'   \code{metric_set} and \code{purpose}.
#' @param purpose Optional character scalar. Selects all metrics tagged for a purpose
#'   (used only when \code{metrics} and \code{metric_set} are NULL).
#' @param summaries Character vector of summary stat names (e.g., \code{c("med","iqr")})
#'   or a named list of summary functions.
#' @param return_annual Logical. If TRUE, also returns the annual base metric table.
#'
#' @return
#' A tibble with columns:
#' \itemize{
#' \item \code{metric_id}: legacy ERFA metric id (1..70) when applicable; otherwise NA
#' \item \code{metric_key}: base metric key (stable join key)
#' \item \code{group}: metric group
#' \item \code{stat}: summary statistic label (e.g., "med", "iqr")
#' \item \code{metric}: legacy-style name (e.g., "amax1d_m", "amax1d_v")
#' \item \code{value}: summarized metric value
#' }
#' For multi-site input, includes \code{site}. If \code{return_annual = TRUE}, returns
#' a list with elements \code{summary} and \code{annual}. The annual table includes
#' \code{year}, \code{metric_key}, and \code{value} (plus \code{site} for multi-site).
#'
#' @examples
#' date <- seq.Date(as.Date("2001-01-01"), as.Date("2002-12-31"), by = "day")
#' Q <- 2 + sin(seq_along(date) * 2 * pi / 365)
#' out <- calculate_hydro_metrics(Q, date, metrics = c("amax1d", "amin1d"))
#' out
#'
#' Q_multi <- cbind(site_a = Q, site_b = Q * 1.1)
#' out_multi <- calculate_hydro_metrics(Q_multi, date, metrics = c("amax1d", "amin1d"))
#' out_multi
#' @export
calculate_hydro_metrics <- function(
    Q, date,
    high_thresh_quantile = 0.9,
    low_thresh_quantile  = 0.1,

    threshold_scope = c("global", "annual", "seasonal", "baseline"),
    baseline_years = NULL,
    baseline_dates = NULL,
    min_finite_thresholds = 10L,

    zero_thr_mode = c("absolute", "quantile", "fraction_median"),
    zero_thr = 0.02,
    zero_q = 0.02,
    zero_frac_median = 0.001,

    roc_eps = 0,
    roc_smooth_k = 1L,

    month_min_frac = 0,
    report_coverage = FALSE,

    metric_set = "erfa70",
    metrics = NULL,
    purpose = NULL,
    summaries = c("med", "iqr"),
    return_annual = FALSE
) {


  .hydro_validate_registry(strict = TRUE)


  threshold_scope <- match.arg(threshold_scope)
  zero_thr_mode <- match.arg(zero_thr_mode)

  if (!is.numeric(month_min_frac) || length(month_min_frac) != 1L || is.na(month_min_frac) ||
      month_min_frac < 0 || month_min_frac > 1) {
    stop("month_min_frac must be a numeric scalar in [0,1].", call. = FALSE)
  }

  if (!is.numeric(roc_eps) || length(roc_eps) != 1L || is.na(roc_eps) || roc_eps < 0) {
    stop("roc_eps must be a numeric scalar >= 0.", call. = FALSE)
  }

  roc_smooth_k <- as.integer(roc_smooth_k)
  if (is.na(roc_smooth_k) || roc_smooth_k < 1L) {
    stop("roc_smooth_k must be an integer >= 1.", call. = FALSE)
  }

  min_finite_thresholds <- as.integer(min_finite_thresholds)
  if (is.na(min_finite_thresholds) || min_finite_thresholds < 1L) {
    stop("min_finite_thresholds must be an integer >= 1.", call. = FALSE)
  }

  metric_keys <- .hydro_resolve_metric_keys(metric_set = metric_set, metrics = metrics, purpose = purpose)
  summary_fns <- .hydro_resolve_summaries(summaries)

  is_multi <- !is.vector(Q)

  datev <- .hydro_as_date(date, name = "date")
  .hydro_validate_daily_dates(datev)
  .hydro_validate_baseline_spec(threshold_scope, baseline_years, baseline_dates, datev)

  # Empty input guard
  if (length(datev) == 0L) {
    if (is_multi) {
      site_names <- colnames(Q)
      if (is.null(site_names)) site_names <- paste0("site", seq_len(ncol(Q)))
      out_list <- vector("list", length(site_names))
      for (i in seq_along(site_names)) {
        out_list[[i]] <- .hydro_empty_result(metric_keys, site = site_names[i], expanded = TRUE, summaries = summaries)
      }
      summary_tbl <- dplyr::bind_rows(out_list)
      if (!return_annual) return(summary_tbl)
      return(list(summary = summary_tbl, annual = tibble::tibble()))
    }

    summary_tbl <- .hydro_empty_result(metric_keys, expanded = TRUE, summaries = summaries)
    if (!return_annual) return(summary_tbl)
    return(list(summary = summary_tbl, annual = tibble::tibble()))
  }

  calc_site <- function(Q_site, site_name = NULL) {
    if (length(Q_site) != length(datev)) {
      stop("Length mismatch: discharge and date must have the same length.", call. = FALSE)
    }

    Q_site <- .hydro_as_numeric(Q_site, name = "Q")

    yr_all   <- lubridate::year(datev)
    mon_all  <- lubridate::month(datev)
    jday_all <- lubridate::yday(datev)

    # Precompute thresholds (global/seasonal/baseline). Annual handled per year.
    thr_obj <- .hydro_build_threshold_object(
      Qv = Q_site,
      datev = datev,
      threshold_scope = threshold_scope,
      high_q = high_thresh_quantile,
      low_q = low_thresh_quantile,
      zero_thr_mode = zero_thr_mode,
      zero_thr = zero_thr,
      zero_q = zero_q,
      zero_frac_median = zero_frac_median,
      min_finite_thresholds = min_finite_thresholds,
      baseline_years = baseline_years,
      baseline_dates = baseline_dates
    )

    reg_all <- .hydro_metric_registry_base()
    reg_use <- reg_all[match(metric_keys, reg_all$metric_key), , drop = FALSE]

    years <- sort(unique(yr_all))
    ny <- length(years)
    annual_rows <- vector("list", ny)

    for (i in seq_len(ny)) {
      y <- years[i]
      idx <- which(yr_all == y)
      if (length(idx) == 0L) next

      Qy   <- Q_site[idx]
      jdy  <- jday_all[idx]
      mony <- mon_all[idx]

      # Determine thresholds aligned with this year's daily values
      if (identical(thr_obj$scope, "annual")) {
        hl <- .hydro_compute_hilo_thresholds(Qy, high_thresh_quantile, low_thresh_quantile, min_finite_thresholds)
        z  <- .hydro_compute_zero_threshold(Qy, zero_thr_mode, zero_thr, zero_q, zero_frac_median, min_finite_thresholds)
        thr_high_vec <- rep(hl$high_thr, length(Qy))
        thr_low_vec  <- rep(hl$low_thr,  length(Qy))
        thr_zero_vec <- rep(z,           length(Qy))
      } else if (identical(thr_obj$scope, "seasonal")) {
        thr_high_vec <- as.numeric(thr_obj$high[as.character(mony)])
        thr_low_vec  <- as.numeric(thr_obj$low[as.character(mony)])
        thr_zero_vec <- as.numeric(thr_obj$zero[as.character(mony)])
      } else {
        # global or baseline => scalars
        thr_high_vec <- rep(thr_obj$high, length(Qy))
        thr_low_vec  <- rep(thr_obj$low,  length(Qy))
        thr_zero_vec <- rep(thr_obj$zero, length(Qy))
      }

      ctx <- .hydro_annual_context(
        Qy = Qy, jdy = jdy, mony = mony, y = y,
        thr_high_vec = thr_high_vec,
        thr_low_vec = thr_low_vec,
        thr_zero_vec = thr_zero_vec,
        roc_eps = roc_eps,
        roc_smooth_k = roc_smooth_k,
        month_min_frac = month_min_frac,
        report_coverage = report_coverage
      )

      use_compute <- vapply(reg_use$compute, is.function, logical(1))
      if (any(!use_compute)) {
        stop("Registry compute functions are missing for selected metrics.", call. = FALSE)
      }

      vals <- numeric(length(metric_keys))
      for (k in seq_along(metric_keys)) {
        vals[k] <- reg_use$compute[[k]](ctx)
      }
      names(vals) <- metric_keys

      annual_rows[[i]] <- tibble::tibble(
        year = y,
        metric_key = metric_keys,
        value = as.numeric(vals)
      )
    }

    annual_tbl <- dplyr::bind_rows(annual_rows)
    if (nrow(annual_tbl) == 0L) {
      summary_tbl <- .hydro_empty_result(metric_keys, expanded = TRUE, summaries = summaries)
      if (!is.null(site_name)) {
        summary_tbl$site <- site_name
        summary_tbl <- dplyr::relocate(summary_tbl, site, .before = metric_key)
      }
      return(list(summary = summary_tbl, annual = annual_tbl))
    }

    stat_names <- names(summary_fns)
    sum_list <- vector("list", length(stat_names))

    reg <- .hydro_metric_registry_base()
    reg <- reg[match(metric_keys, reg$metric_key), , drop = FALSE]
    reg$group <- factor(reg$group, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE)

    for (si in seq_along(stat_names)) {
      st <- stat_names[si]
      fn <- summary_fns[[st]]

      vals <- numeric(length(metric_keys))
      for (k in seq_along(metric_keys)) {
        mk <- metric_keys[k]
        x <- annual_tbl$value[annual_tbl$metric_key == mk]
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

    summary_tbl <- dplyr::bind_rows(sum_list)

    # Legacy ERFA metric_id mapping only for full ERFA set + med/iqr
    is_erfa_set <- identical(sort(metric_keys), sort(.hydro_metric_sets()[["erfa70"]]))
    has_erfa_stats <- identical(sort(names(summary_fns)), sort(c("med", "iqr")))

    if (is_erfa_set && has_erfa_stats) {
      erfa70 <- .hydro_registry_erfa70_expanded()
      summary_tbl <- dplyr::left_join(
        summary_tbl,
        erfa70,
        by = c("metric_key", "stat", "group", "metric")
      )
      summary_tbl <- dplyr::relocate(summary_tbl, metric_id, .before = metric_key)
    } else {
      summary_tbl$metric_id <- NA_integer_
      summary_tbl <- dplyr::relocate(summary_tbl, metric_id, .before = metric_key)
    }

    if (!is.null(site_name)) {
      summary_tbl$site <- site_name
      summary_tbl <- dplyr::relocate(summary_tbl, site, .before = metric_id)

      annual_tbl$site <- site_name
      annual_tbl <- dplyr::relocate(annual_tbl, site, .before = year)
    }

    list(summary = summary_tbl, annual = annual_tbl)
  }

  # Multi-site
  if (is_multi) {
    if (!is.matrix(Q) && !is.data.frame(Q)) {
      stop("Q must be a vector, matrix, or data.frame", call. = FALSE)
    }
    if (nrow(Q) != length(datev)) {
      stop("nrow(Q) must equal length(date)", call. = FALSE)
    }

    site_names <- colnames(Q)
    if (is.null(site_names)) site_names <- paste0("site", seq_len(ncol(Q)))

    sum_out <- vector("list", ncol(Q))
    ann_out <- vector("list", ncol(Q))

    for (i in seq_len(ncol(Q))) {
      res_i <- calc_site(Q[, i], site_name = site_names[i])
      sum_out[[i]] <- res_i$summary
      ann_out[[i]] <- res_i$annual
    }

    summary_tbl <- dplyr::bind_rows(sum_out)
    annual_tbl <- dplyr::bind_rows(ann_out)

    if (!return_annual) return(summary_tbl)
    return(list(summary = summary_tbl, annual = annual_tbl))
  }

  # Single-site
  res <- calc_site(Q, site_name = NULL)
  if (!return_annual) return(res$summary)
  res
}
