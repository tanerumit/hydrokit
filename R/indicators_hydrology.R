# ==============================================================================
# Script: R/eflow_metrics.R
# Purpose: Registry-backed hydrological metric computation and summarization.
#
# Exported functions:
#   - hydro_control()
#   - calculate_hydro_metrics()
#   - summarize_hydro_metrics()
#   - list_hydro_metrics()
#
# Design principles:
#   - Registry encodes temporal grain per metric (annual vs period)
#   - calculate_hydro_metrics() returns raw values at their natural grain
#   - summarize_hydro_metrics() handles across-year summarization + ERFA mapping
#   - hydro_control() groups expert tuning parameters
# ==============================================================================

# ------------------------------------------------------------------------------
# REGISTRY CACHE
# ------------------------------------------------------------------------------
.hydro_cache <- new.env(parent = emptyenv())

#' Invalidate the cached metric registry
#'
#' Forces the next call to \code{.hydro_metric_registry_base()} (and all
#' functions that depend on it) to rebuild the registry from scratch. Useful
#' after hot-patching registry definitions during development.
#'
#' @return Invisible \code{TRUE}.
#' @keywords internal
.hydro_registry_invalidate <- function() {
  rm(list = ls(envir = .hydro_cache), envir = .hydro_cache)
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# CENTRAL REGISTRY (HYDROLOGICAL BASE METRICS)
# ------------------------------------------------------------------------------

#' Build (or retrieve from cache) the base metric registry.
#' @return Tibble with metric definitions.
#' @keywords internal
.hydro_metric_registry_base <- function() {
  if (exists("registry_base", envir = .hydro_cache, inherits = FALSE)) {
    return(base::get("registry_base", envir = .hydro_cache, inherits = FALSE))
  }
  reg <- .hydro_metric_registry_base_build()
  assign("registry_base", reg, envir = .hydro_cache)
  reg
}

#' Internal: construct the base metric registry from scratch.
#' @return Tibble with metric definitions including grain and purpose list-column.
#' @keywords internal
.hydro_metric_registry_base_build <- function() {

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
    )
  )

  # ---- purposes list-column
  purposes <- vector("list", nrow(reg))

  for (i in seq_len(nrow(reg))) {
    purposes[[i]] <- c("hydrology", "eflow", "erfa")
  }

  add_tags <- function(keys, tags) {
    ii <- match(keys, reg$metric_key)
    ii <- ii[!is.na(ii)]
    for (j in ii) {
      purposes[[j]] <<- unique(c(purposes[[j]], tags))
    }
  }

  add_tags(c("amax1d","amax3d","amax7d","amax30d","amax90d"), c("navigation","energy"))
  add_tags(c("juldate_amax1d","num_high_pulses","avg_dur_high_pulses"), c("navigation"))
  add_tags(c(
    "january_flow","february_flow","march_flow","april_flow","may_flow","june_flow",
    "july_flow","august_flow","september_flow","october_flow","november_flow","december_flow"
  ), c("irrigation","energy","navigation"))
  add_tags(c("amin1d","amin3d","amin7d","amin30d","amin90d"), c("irrigation","navigation","energy"))
  add_tags(c("juldate_amin1d","num_low_pulses","avg_dur_low_pulses"), c("irrigation","navigation"))
  add_tags(c("mean_rate_rise","mean_rate_fall","num_rises","num_falls"), c("energy","navigation"))
  add_tags(c("num_zero_seq","avg_dur_zero_seq","time_main_zero_seq"), c("irrigation","energy","navigation"))

  reg$purposes <- purposes

  # ---- compute handlers
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

# ------------------------------------------------------------------------------
# VALIDATE REGISTRY
# ------------------------------------------------------------------------------

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
  required_cols <- c("metric_key", "group", "grain", "label", "purposes", "compute", "requires")
  missing_cols <- setdiff(required_cols, names(reg))
  if (length(missing_cols) > 0L) {
    .fail("Registry missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.character(reg$metric_key) || anyNA(reg$metric_key) || any(reg$metric_key == "")) {
    .fail("Registry metric_key must be non-empty character values without NA.")
  }

  if (anyDuplicated(reg$metric_key) > 0L) {
    dups <- unique(reg$metric_key[duplicated(reg$metric_key)])
    .fail("Duplicate metric_key values in registry: ", paste(dups, collapse = ", "))
  }

  # Group sanity
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
    .warn("Registry contains metrics with group = NA. ERFA-style grouping will not apply.")
  }

  # Grain checks
  allowed_grains <- c("annual", "period")
  bad_grain <- !reg$grain %in% allowed_grains
  if (any(bad_grain)) {
    .fail(
      "Registry contains unknown grain values: ",
      paste(unique(reg$grain[bad_grain]), collapse = ", "),
      ". Allowed: ", paste(allowed_grains, collapse = ", "), "."
    )
  }

  # Purposes
  if (!is.list(reg$purposes)) {
    .fail("Registry purposes must be a list-column of character vectors.")
  }
  bad_purpose <- vapply(reg$purposes, function(x) {
    !(is.character(x) && length(x) >= 1L && all(!is.na(x)) && all(nzchar(x)))
  }, logical(1))
  if (any(bad_purpose)) {
    .fail("Registry purposes must be non-empty character vectors. Bad rows: ",
          paste(which(bad_purpose), collapse = ", "))
  }

  has_dup_purpose <- vapply(reg$purposes, function(x) length(unique(x)) != length(x), logical(1))
  if (any(has_dup_purpose)) {
    .warn("Some metrics have duplicated purpose tags: ",
          paste(reg$metric_key[has_dup_purpose], collapse = ", "))
  }

  # Compute
  if (!is.list(reg$compute) || length(reg$compute) != nrow(reg)) {
    .fail("Registry compute must be a list-column aligned to registry rows.")
  }
  bad_compute <- vapply(reg$compute, function(x) !is.null(x) && !is.function(x), logical(1))
  if (any(bad_compute)) {
    .fail("Registry compute entries must be functions or NULL. Bad: ",
          paste(reg$metric_key[bad_compute], collapse = ", "))
  }

  # Requires
  if (!is.list(reg$requires) || length(reg$requires) != nrow(reg)) {
    .fail("Registry requires must be a list-column aligned to registry rows.")
  }
  bad_requires <- vapply(reg$requires, function(x) {
    !(is.character(x) && all(!is.na(x)) && all(nzchar(x)))
  }, logical(1))
  if (any(bad_requires)) {
    .fail("Registry requires entries must be character vectors. Bad: ",
          paste(reg$metric_key[bad_requires], collapse = ", "))
  }

  # ---- Metric sets checks
  sets <- .hydro_metric_sets()
  if (!is.list(sets) || length(sets) == 0L || is.null(names(sets))) {
    .fail(".hydro_metric_sets() must return a named list of character vectors.")
  }
  for (nm in names(sets)) {
    s <- sets[[nm]]
    if (!is.character(s) || length(s) == 0L || anyNA(s) || any(s == "")) {
      .fail("Metric set '", nm, "' must be a non-empty character vector.")
    }
    unknown <- setdiff(s, reg$metric_key)
    if (length(unknown) > 0L) {
      .fail("Metric set '", nm, "' contains unknown metric_key values: ",
            paste(unknown, collapse = ", "))
    }
    if (anyDuplicated(s) > 0L) {
      .fail("Metric set '", nm, "' contains duplicate metric_key values.")
    }
    idx <- match(s, reg$metric_key)
    compute_ok <- vapply(reg$compute[idx], is.function, logical(1))
    if (any(!compute_ok)) {
      .fail("Metric set '", nm, "' contains metrics without compute functions: ",
            paste(s[!compute_ok], collapse = ", "))
    }
  }

  # ---- ERFA70 expanded mapping checks
  if ("erfa70" %in% names(sets)) {
    if (!identical(sets[["erfa70"]], .hydro_erfa70_base_order())) {
      .fail("ERFA70 metric set ordering must match .hydro_erfa70_base_order().")
    }

    erfa70_exp <- .hydro_registry_erfa70_expanded()
    req_erfa_cols <- c("metric_id", "group", "metric_key", "stat", "metric")
    miss_erfa_cols <- setdiff(req_erfa_cols, names(erfa70_exp))
    if (length(miss_erfa_cols) > 0L) {
      .fail("ERFA70 expanded registry missing columns: ",
            paste(miss_erfa_cols, collapse = ", "))
    }

    if (!is.integer(erfa70_exp$metric_id)) {
      erfa70_exp$metric_id <- as.integer(erfa70_exp$metric_id)
    }
    if (anyNA(erfa70_exp$metric_id) || anyDuplicated(erfa70_exp$metric_id) > 0L) {
      .fail("ERFA70 metric_id must be unique and non-NA.")
    }
    if (!identical(sort(erfa70_exp$metric_id), seq_len(nrow(erfa70_exp)))) {
      .warn("ERFA70 metric_id is not a contiguous 1..N sequence.")
    }

    base_keys <- sets[["erfa70"]]
    n_expected <- length(base_keys) * 2L
    if (nrow(erfa70_exp) != n_expected) {
      .fail("ERFA70 expanded registry row count mismatch: expected ", n_expected,
            ", got ", nrow(erfa70_exp), ".")
    }
    if (!all(erfa70_exp$stat %in% c("med", "iqr"))) {
      .fail("ERFA70 expanded registry stat must be 'med' or 'iqr' only.")
    }

    suffix_ok <- (erfa70_exp$stat == "med" & grepl("_m$", erfa70_exp$metric)) |
      (erfa70_exp$stat == "iqr" & grepl("_v$", erfa70_exp$metric))
    if (!all(suffix_ok)) {
      .fail("ERFA70 expanded registry has inconsistent metric suffixes for med/iqr.")
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# METRIC SETS
# ------------------------------------------------------------------------------

#' Return the named list of metric sets used for selection.
#' @return Named list of character vectors of metric keys.
#' @keywords internal
.hydro_metric_sets <- function() {
  list(erfa70 = .hydro_erfa70_base_order())
}

# ------------------------------------------------------------------------------
# ERFA70 EXPANDED REGISTRY
# ------------------------------------------------------------------------------

#' Expand the ERFA70 registry to include med/iqr legacy metric names.
#' @return Tibble mapping metric_id, metric_key, stat, and group.
#' @keywords internal
.hydro_registry_erfa70_expanded <- function() {
  if (exists("erfa70_expanded", envir = .hydro_cache, inherits = FALSE)) {
    return(base::get("erfa70_expanded", envir = .hydro_cache, inherits = FALSE))
  }

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

  out <- tibble::tibble(
    metric_id = seq_len(length(metric_key_vec)),
    group = factor(group_vec, levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
    metric_key = metric_key_vec,
    stat = stat_vec,
    metric = legacy_metric
  )
  assign("erfa70_expanded", out, envir = .hydro_cache)
  out
}

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
# METRIC SELECTION RESOLVER (simplified: single `metrics` argument)
# ------------------------------------------------------------------------------

#' Resolve requested metric keys.
#'
#' Accepts a character vector of explicit keys, a single registered set name
#' (e.g. "erfa70"), or NULL (defaults to "erfa70").
#'
#' @param metrics Character vector of metric keys, a single set name, or NULL.
#' @return Character vector of metric keys to compute.
#' @keywords internal
.hydro_resolve_metrics <- function(metrics = NULL) {
  reg <- .hydro_metric_registry_base()
  all_keys <- reg$metric_key
  sets <- .hydro_metric_sets()

  # NULL → default set

  if (is.null(metrics)) {
    return(sets[["erfa70"]])
  }

  metrics <- as.character(metrics)

  # Single string matching a registered set name
  if (length(metrics) == 1L && metrics %in% names(sets)) {
    return(sets[[metrics]])
  }

  # Explicit key vector
  unknown <- setdiff(metrics, all_keys)
  if (length(unknown) > 0L) {
    stop("Unknown metrics: ", paste(unknown, collapse = ", "),
         ". Use list_hydro_metrics() to see available keys.", call. = FALSE)
  }
  metrics
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
#' @param month_min_frac Numeric scalar in [0,1]. Minimum fraction of calendar
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

# ==============================================================================
# EXPORTED: list_hydro_metrics()
# ==============================================================================

#' List available hydrological metrics
#'
#' Returns the metric registry as a user-friendly tibble for inspection.
#'
#' @param purpose Optional character scalar. If provided, filters to metrics
#'   tagged with this purpose (e.g. "irrigation", "energy").
#'
#' @return Tibble with columns: metric_key, group, grain, label.
#' @export
#'
#' @examples
#' list_hydro_metrics()
#' list_hydro_metrics(purpose = "irrigation")
list_hydro_metrics <- function(purpose = NULL) {
  reg <- .hydro_metric_registry_base()
  out <- reg[, c("metric_key", "group", "grain", "label")]

  if (!is.null(purpose)) {
    purpose <- as.character(purpose)[1]
    keep <- vapply(reg$purposes, function(x) purpose %in% x, logical(1))
    out <- out[keep, , drop = FALSE]
    if (nrow(out) == 0L) {
      warning("No metrics found for purpose: ", purpose, call. = FALSE)
    }
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
#'   \item A registered set name (e.g. \code{"erfa70"}) — selects the predefined set
#'   \item A character vector of metric keys (e.g. \code{c("amax1d", "amin7d")})
#'   \item \code{NULL} (default) — uses the \code{"erfa70"} set
#' }
#' Use \code{\link{list_hydro_metrics}} to see available keys.
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
#'   and ERFA mapping, \code{\link{list_hydro_metrics}} for available metrics,
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

  metric_keys <- .hydro_resolve_metrics(metrics)
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

    reg_all <- .hydro_metric_registry_base()
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

# ------------------------------------------------------------------------------
# INTERNAL: empty result for annual-grain metrics
# ------------------------------------------------------------------------------

#' @keywords internal
.hydro_empty_annual_result <- function(metric_keys) {
  reg <- .hydro_metric_registry_base()
  reg <- reg[match(metric_keys, reg$metric_key), , drop = FALSE]
  tibble::tibble(
    year = integer(0),
    metric_key = character(0),
    group = factor(character(0), levels = c("HF", "MF", "LF", "RFC", "IF"), ordered = TRUE),
    grain = character(0),
    value = numeric(0)
  )
}

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
  summary_fns <- .hydro_resolve_summaries(summaries)
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
    reg <- .hydro_metric_registry_base()
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
  is_erfa_set <- identical(sort(metric_keys_used), sort(.hydro_metric_sets()[["erfa70"]]))
  has_erfa_stats <- identical(sort(stat_names), sort(c("med", "iqr")))

  if (is_erfa_set && has_erfa_stats) {
    erfa70 <- .hydro_registry_erfa70_expanded()
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
