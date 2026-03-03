# ==============================================================================
# Script: R/flow-registry.R
# Purpose: Registry cache, build, validation, and metric resolving for flow metrics.
# ==============================================================================

# ------------------------------------------------------------------------------
# REGISTRY CACHE
# ------------------------------------------------------------------------------
.flow_cache <- new.env(parent = emptyenv())

#' Invalidate cached flow registry objects
#'
#' @return Invisible \code{TRUE}.
#' @keywords internal
.flow_registry_invalidate <- function() {
  rm(list = ls(envir = .flow_cache), envir = .flow_cache)
  invisible(TRUE)
}

#' Build (or retrieve) flow metric registry
#'
#' @return Tibble with flow metric metadata and runtime fields.
#' @keywords internal
.flow_metric_registry <- function() {
  if (exists("registry_base", envir = .flow_cache, inherits = FALSE)) {
    return(base::get("registry_base", envir = .flow_cache, inherits = FALSE))
  }
  reg <- .flow_metric_registry_build()
  assign("registry_base", reg, envir = .flow_cache)
  reg
}

#' Build flow metric registry from flow catalog
#'
#' @return Tibble with columns:
#'   metric_key, group, grain, label, purposes, compute, requires.
#' @keywords internal
.flow_metric_registry_build <- function() {
  catalog <- .metric_catalog_flow()

  reg <- tibble::tibble(
    metric_key = catalog$key,
    group = catalog$group,
    grain = catalog$grain,
    label = catalog$label
  )

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

  add_tags(c("amax1d", "amax3d", "amax7d", "amax30d", "amax90d"), c("navigation", "energy"))
  add_tags(c("juldate_amax1d", "num_high_pulses", "avg_dur_high_pulses"), c("navigation"))
  add_tags(c(
    "january_flow", "february_flow", "march_flow", "april_flow", "may_flow", "june_flow",
    "july_flow", "august_flow", "september_flow", "october_flow", "november_flow", "december_flow"
  ), c("irrigation", "energy", "navigation"))
  add_tags(c("amin1d", "amin3d", "amin7d", "amin30d", "amin90d"), c("irrigation", "navigation", "energy"))
  add_tags(c("juldate_amin1d", "num_low_pulses", "avg_dur_low_pulses"), c("irrigation", "navigation"))
  add_tags(c("mean_rate_rise", "mean_rate_fall", "num_rises", "num_falls"), c("energy", "navigation"))
  add_tags(c("num_zero_seq", "avg_dur_zero_seq", "time_main_zero_seq"), c("irrigation", "energy", "navigation"))
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

#' Resolve named flow metric sets
#'
#' @return Named list of metric key vectors.
#' @keywords internal
.flow_metric_sets <- function() {
  list(erfa70 = .resolve_metric_set("erfa70")$keys)
}

#' Resolve requested flow metric keys
#'
#' @param metrics Character vector of metric keys, a set name, or \code{NULL}.
#' @return Character vector of metric keys.
#' @keywords internal
.flow_resolve_metrics <- function(metrics = NULL) {
  reg <- .flow_metric_registry()
  all_keys <- reg$metric_key
  sets <- .flow_metric_sets()

  if (is.null(metrics)) {
    return(sets[["erfa70"]])
  }

  metrics <- as.character(metrics)
  if (length(metrics) == 1L && metrics %in% names(sets)) {
    return(sets[[metrics]])
  }

  unknown <- setdiff(metrics, all_keys)
  if (length(unknown) > 0L) {
    stop("Unknown metrics: ", paste(unknown, collapse = ", "),
         ". Use list_metrics(family = 'flow') to see available keys.", call. = FALSE)
  }
  metrics
}

#' Validate flow registry structure and metric sets
#'
#' @param strict Logical; treat warnings as errors when \code{TRUE}.
#' @return Invisible \code{TRUE}.
#' @keywords internal
.flow_validate_registry <- function(strict = FALSE) {
  .fail <- function(...) stop(..., call. = FALSE)
  .warn <- function(...) {
    msg <- paste0(...)
    if (isTRUE(strict)) .fail(msg)
    warning(msg, call. = FALSE)
  }

  reg <- .flow_metric_registry()
  required_cols <- c("metric_key", "group", "grain", "label", "compute", "requires")
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

  allowed_groups <- c("HF", "MF", "LF", "RFC", "IF")
  bad_group <- !is.na(reg$group) & !reg$group %in% allowed_groups
  if (any(bad_group)) {
    .fail("Registry contains unknown group values: ", paste(unique(reg$group[bad_group]), collapse = ", "))
  }
  if (any(is.na(reg$group))) {
    .warn("Registry contains metrics with group = NA. ERFA-style grouping will not apply.")
  }

  allowed_grains <- c("annual", "period")
  bad_grain <- !reg$grain %in% allowed_grains
  if (any(bad_grain)) {
    .fail("Registry contains unknown grain values: ", paste(unique(reg$grain[bad_grain]), collapse = ", "))
  }

  bad_compute <- vapply(reg$compute, function(x) !is.null(x) && !is.function(x), logical(1))
  if (any(bad_compute)) {
    .fail("Registry compute entries must be functions or NULL. Bad: ",
          paste(reg$metric_key[bad_compute], collapse = ", "))
  }

  bad_requires <- vapply(reg$requires, function(x) {
    !(is.character(x) && all(!is.na(x)) && all(nzchar(x)))
  }, logical(1))
  if (any(bad_requires)) {
    .fail("Registry requires entries must be character vectors. Bad: ",
          paste(reg$metric_key[bad_requires], collapse = ", "))
  }

  set <- .resolve_metric_set("erfa70")
  unknown <- setdiff(set$keys, reg$metric_key)
  if (length(unknown) > 0L) {
    .fail("Metric set 'erfa70' contains unknown metric_key values: ",
          paste(unknown, collapse = ", "))
  }
  if (anyDuplicated(set$keys) > 0L) {
    .fail("Metric set 'erfa70' contains duplicate metric_key values.")
  }
  idx <- match(set$keys, reg$metric_key)
  compute_ok <- vapply(reg$compute[idx], is.function, logical(1))
  if (any(!compute_ok)) {
    .fail("Metric set 'erfa70' contains metrics without compute functions: ",
          paste(set$keys[!compute_ok], collapse = ", "))
  }

  invisible(TRUE)
}

# Compatibility aliases for current internal call sites.
.hydro_registry_invalidate <- .flow_registry_invalidate
.hydro_metric_registry_base <- .flow_metric_registry
.hydro_metric_registry_base_build <- .flow_metric_registry_build
.hydro_metric_sets <- .flow_metric_sets
.hydro_resolve_metrics <- .flow_resolve_metrics
.hydro_validate_registry <- .flow_validate_registry
.hydro_registry_erfa70_expanded <- .flow_registry_erfa70_expanded
.hydro_erfa70_base_order <- function() .metric_set_erfa70()$keys
