# =============================================================================
# Hashimoto-style performance metrics for water supply systems
# =============================================================================
#
# Metrics implemented:
# - reliability_hashimoto()
# - resilience_hashimoto()
# - vulnerability_hashimoto()
# - failure_frequency()
# - mean_failure_duration()
# - mean_recovery_time()
#
# Shared conventions:
# - success: sim_supply >= obs_threshold
# - failure: sim_supply < obs_threshold
# - na_action: "omit" drops non-finite pairs; "fail" errors
# =============================================================================

# ---- internal helpers --------------------------------------------------------

.stop_scalar_chr_ <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    stop(sprintf("`%s` must be a non-empty character scalar.", name))
  }
}

.stop_same_length_numeric_ <- function(x, y, x_name, y_name) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop(sprintf("`%s` and `%s` must be numeric vectors.", x_name, y_name))
  }
  if (length(x) != length(y)) {
    stop(sprintf("`%s` and `%s` must have the same length.", x_name, y_name))
  }
}

.state_from_threshold_supply_ <- function(obs_threshold,
                                          sim_supply,
                                          na_action = c("omit", "fail")) {

  na_action <- match.arg(na_action)

  .stop_same_length_numeric_(obs_threshold, sim_supply, "obs_threshold", "sim_supply")

  n_in <- length(obs_threshold)
  if (n_in == 0L) {
    return(list(
      is_success = logical(0),
      is_fail = logical(0),
      sim_deficit = numeric(0),
      n_steps = 0L,
      n_dropped = 0L
    ))
  }

  idx_ok <- is.finite(obs_threshold) & is.finite(sim_supply)
  n_dropped <- sum(!idx_ok)

  if (n_dropped > 0L && na_action == "fail") {
    stop("Non-finite values found in `obs_threshold` or `sim_supply`.")
  }

  if (na_action == "omit") {
    obs_threshold <- obs_threshold[idx_ok]
    sim_supply <- sim_supply[idx_ok]
  }

  # state definition
  is_success <- sim_supply >= obs_threshold
  is_fail <- !is_success

  # deficit magnitude (0 in success, positive in failure)
  sim_deficit <- pmax(obs_threshold - sim_supply, 0)

  list(
    is_success = is_success,
    is_fail = is_fail,
    sim_deficit = sim_deficit,
    n_steps = length(is_success),
    n_dropped = as.integer(n_dropped)
  )
}

.fail_run_lengths_ <- function(is_fail) {
  if (length(is_fail) == 0L) return(integer(0))
  r <- rle(is_fail)
  as.integer(r$lengths[r$values])
}

.n_fail_events_ <- function(is_fail) {
  if (length(is_fail) == 0L) return(0L)
  r <- rle(is_fail)
  as.integer(sum(r$values))
}

# Transition-aligned success/failure for resilience
.transitions_from_threshold_supply_ <- function(obs_threshold,
                                                sim_supply,
                                                na_action = c("omit", "fail")) {

  na_action <- match.arg(na_action)

  .stop_same_length_numeric_(obs_threshold, sim_supply, "obs_threshold", "sim_supply")

  n_in <- length(obs_threshold)
  if (n_in < 2L) {
    return(list(
      is_fail_t = logical(0),
      is_success_tp1 = logical(0),
      n_transitions = 0L,
      n_dropped = as.integer(n_in)
    ))
  }

  idx_ok_step <- is.finite(obs_threshold) & is.finite(sim_supply)

  # transitions need both t and t+1 finite
  idx_ok_tr <- idx_ok_step[-n_in] & idx_ok_step[-1L]
  n_dropped <- sum(!idx_ok_tr)

  if (n_dropped > 0L && na_action == "fail") {
    stop("Non-finite values found in `obs_threshold` or `sim_supply` affecting transitions.")
  }

  obs_thr_t   <- obs_threshold[-n_in]
  sim_sup_t   <- sim_supply[-n_in]
  obs_thr_tp1 <- obs_threshold[-1L]
  sim_sup_tp1 <- sim_supply[-1L]

  if (na_action == "omit") {
    obs_thr_t   <- obs_thr_t[idx_ok_tr]
    sim_sup_t   <- sim_sup_t[idx_ok_tr]
    obs_thr_tp1 <- obs_thr_tp1[idx_ok_tr]
    sim_sup_tp1 <- sim_sup_tp1[idx_ok_tr]
  }

  is_success_t   <- sim_sup_t   >= obs_thr_t
  is_success_tp1 <- sim_sup_tp1 >= obs_thr_tp1

  list(
    is_fail_t = !is_success_t,
    is_success_tp1 = is_success_tp1,
    n_transitions = length(is_success_tp1),
    n_dropped = as.integer(n_dropped)
  )
}

# ---- exported metrics --------------------------------------------------------

#' Hashimoto reliability
#'
#' Reliability is the long-run probability of satisfactory performance:
#' \deqn{R = P(S_t = 1)}
#' where \eqn{S_t = 1} denotes success.
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_data Character scalar: value if no valid timesteps remain: "na" (default) or "zero".
#'
#' @return Numeric scalar in [0,1] (or NA_real_).
#' @export
#'
#' @examples
#' obs_threshold <- rep(10, 5)
#' sim_supply <- c(12, 8, 10, 11, 9)
#' reliability_hashimoto(obs_threshold, sim_supply)
reliability_hashimoto <- function(obs_threshold,
                                  sim_supply,
                                  na_action = c("omit", "fail"),
                                  no_data = c("na", "zero")) {

  no_data <- match.arg(no_data)
  st <- .state_from_threshold_supply_(obs_threshold, sim_supply, na_action = na_action)

  if (st$n_steps == 0L) {
    return(if (no_data == "na") NA_real_ else 0)
  }

  mean(st$is_success)
}

#' Hashimoto resilience
#'
#' Resilience is the conditional probability of returning to success at the next
#' timestep given failure at the current timestep:
#' \deqn{Re = P(S_{t+1}=1 \mid S_t=0)}.
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_failure Character scalar: value if there are zero eligible failure states:
#'   "na" (default), "one", or "zero".
#'
#' @return Numeric scalar in [0,1] (or NA_real_).
#' @export
#'
#' @examples
#' obs_threshold <- rep(10, 6)
#' sim_supply <- c(12, 8, 7, 11, 9, 10)
#' resilience_hashimoto(obs_threshold, sim_supply)
resilience_hashimoto <- function(obs_threshold,
                                 sim_supply,
                                 na_action = c("omit", "fail"),
                                 no_failure = c("na", "one", "zero")) {

  no_failure <- match.arg(no_failure)

  tr <- .transitions_from_threshold_supply_(obs_threshold, sim_supply, na_action = na_action)

  if (tr$n_transitions == 0L) return(NA_real_)

  n_fail_states <- sum(tr$is_fail_t)
  if (n_fail_states == 0L) {
    return(switch(no_failure, na = NA_real_, one = 1, zero = 0))
  }

  n_recover_next <- sum(tr$is_fail_t & tr$is_success_tp1)
  out_resilience <- n_recover_next / n_fail_states
  max(min(out_resilience, 1), 0)
}

#' Hashimoto vulnerability
#'
#' Vulnerability summarizes the severity of failure when failure occurs.
#' This implementation uses deficit magnitude \eqn{M_t = max(threshold - supply, 0)}
#' and returns a summary over failure timesteps:
#' \deqn{V = E(M_t \mid S_t=0)} (default statistic = mean).
#'
#' @details
#' This is the common “state-based” vulnerability variant. If you need event-based
#' vulnerability (e.g., mean of event-integrated deficits), implement separately
#' to avoid mixing definitions.
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param statistic Character scalar: "mean" or "max" applied to deficit magnitudes
#'   over failure timesteps.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_failure Character scalar: value if there are no failures: "na" (default), "zero".
#'
#' @return Numeric scalar (units of supply/demand). Returns NA_real_ if undefined.
#' @export
#'
#' @examples
#' obs_threshold <- rep(10, 5)
#' sim_supply <- c(12, 8, 10, 11, 9)
#' vulnerability_hashimoto(obs_threshold, sim_supply, statistic = "mean")
vulnerability_hashimoto <- function(obs_threshold,
                                    sim_supply,
                                    statistic = c("mean", "max"),
                                    na_action = c("omit", "fail"),
                                    no_failure = c("na", "zero")) {

  statistic <- match.arg(statistic)
  no_failure <- match.arg(no_failure)

  st <- .state_from_threshold_supply_(obs_threshold, sim_supply, na_action = na_action)

  if (st$n_steps == 0L) return(NA_real_)

  if (!any(st$is_fail)) {
    return(if (no_failure == "na") NA_real_ else 0)
  }

  sim_deficit_fail <- st$sim_deficit[st$is_fail]

  if (statistic == "mean") {
    mean(sim_deficit_fail)
  } else {
    max(sim_deficit_fail)
  }
}

#' Failure frequency
#'
#' Failure frequency is the number of distinct failure events (contiguous failure runs)
#' per timestep.
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_data Character scalar: value if no valid timesteps remain: "na" (default) or "zero".
#'
#' @return Numeric scalar (events per timestep) in [0,1] (or NA_real_).
#' @export
failure_frequency <- function(obs_threshold,
                              sim_supply,
                              na_action = c("omit", "fail"),
                              no_data = c("na", "zero")) {

  no_data <- match.arg(no_data)
  st <- .state_from_threshold_supply_(obs_threshold, sim_supply, na_action = na_action)

  if (st$n_steps == 0L) return(if (no_data == "na") NA_real_ else 0)

  n_fail_events <- .n_fail_events_(st$is_fail)
  n_fail_events / st$n_steps
}

#' Mean failure duration
#'
#' Mean duration of contiguous failure events (in timesteps).
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_failure Character scalar: value if there are no failures: "na" (default), "zero".
#'
#' @return Numeric scalar (timesteps) or NA_real_.
#' @export
mean_failure_duration <- function(obs_threshold,
                                  sim_supply,
                                  na_action = c("omit", "fail"),
                                  no_failure = c("na", "zero")) {

  no_failure <- match.arg(no_failure)
  st <- .state_from_threshold_supply_(obs_threshold, sim_supply, na_action = na_action)

  if (st$n_steps == 0L) return(NA_real_)

  fail_runs <- .fail_run_lengths_(st$is_fail)
  if (length(fail_runs) == 0L) return(if (no_failure == "na") NA_real_ else 0)

  mean(fail_runs)
}

#' Mean recovery time
#'
#' Mean number of timesteps to recover to success after entering failure.
#' Under this discrete-time state definition, this equals mean failure duration.
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_failure Character scalar: value if there are no failures: "na" (default), "zero".
#'
#' @return Numeric scalar (timesteps) or NA_real_.
#' @export
mean_recovery_time <- function(obs_threshold,
                               sim_supply,
                               na_action = c("omit", "fail"),
                               no_failure = c("na", "zero")) {

  mean_failure_duration(
    obs_threshold = obs_threshold,
    sim_supply = sim_supply,
    na_action = na_action,
    no_failure = no_failure
  )
}


#' Hashimoto vulnerability (event-based)
#'
#' Computes event-based vulnerability by identifying contiguous failure events
#' (runs where `sim_supply < obs_threshold`) and computing a severity statistic
#' per event from deficit magnitudes. The returned vulnerability is then a summary
#' across events.
#'
#' @details
#' Failure event: contiguous timesteps with `sim_supply < obs_threshold`.
#' Deficit magnitude: `max(obs_threshold - sim_supply, 0)`.
#'
#' Event severity (`event_stat`):
#' - "sum": integrated deficit over the event (units: supply * timestep)
#' - "mean": mean deficit magnitude during the event (units: supply)
#' - "max": maximum deficit magnitude during the event (units: supply)
#'
#' Across-event aggregation (`event_agg`):
#' - "mean" (default), "median", "max"
#'
#' Edge cases:
#' - If there are no failure events, return value controlled by `no_failure`.
#' - Non-finite values handled via `na_action` (consistent with other metrics).
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param event_stat Character scalar: "sum", "mean", or "max" severity per event.
#' @param event_agg Character scalar: "mean", "median", or "max" aggregation across events.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param no_failure Character scalar: value if there are no failure events: "na" (default), "zero".
#' @param return_components Logical. If TRUE, return diagnostics including per-event severities.
#'
#' @return If `return_components = FALSE`, numeric scalar vulnerability (units depend on `event_stat`).
#' If `return_components = TRUE`, a list with:
#' - `vulnerability` numeric scalar,
#' - `n_events` integer,
#' - `event_values` numeric vector (per-event severities),
#' - `event_lengths` integer vector (timesteps per event),
#' - `n_dropped` integer (timesteps dropped due to NA handling).
#' @export
#'
#' @examples
#' obs_threshold <- rep(10, 8)
#' sim_supply <- c(12, 8, 7, 11, 9, 8, 10, 12)
#' vulnerability_event_hashimoto(obs_threshold, sim_supply, event_stat = "sum")
vulnerability_event_hashimoto <- function(obs_threshold,
                                          sim_supply,
                                          event_stat = c("sum", "mean", "max"),
                                          event_agg = c("mean", "median", "max"),
                                          na_action = c("omit", "fail"),
                                          no_failure = c("na", "zero"),
                                          return_components = FALSE) {

  event_stat <- match.arg(event_stat)
  event_agg <- match.arg(event_agg)
  no_failure <- match.arg(no_failure)

  st <- .state_from_threshold_supply_(obs_threshold, sim_supply, na_action = na_action)

  if (st$n_steps == 0L) {
    if (return_components) {
      return(list(
        vulnerability = NA_real_,
        n_events = 0L,
        event_values = numeric(0),
        event_lengths = integer(0),
        n_dropped = st$n_dropped
      ))
    }
    return(NA_real_)
  }

  if (!any(st$is_fail)) {
    out_v <- if (no_failure == "na") NA_real_ else 0
    if (!return_components) return(out_v)

    return(list(
      vulnerability = out_v,
      n_events = 0L,
      event_values = numeric(0),
      event_lengths = integer(0),
      n_dropped = st$n_dropped
    ))
  }

  # Identify failure events on the cleaned series
  rle_fail <- rle(st$is_fail)
  fail_event_lengths <- as.integer(rle_fail$lengths[rle_fail$values])
  n_events <- length(fail_event_lengths)

  # Compute per-event severities by walking the RLE structure
  # (keeps allocations low and avoids rebuilding event indices)
  event_values <- numeric(n_events)

  # positions in the time series
  pos_end <- cumsum(rle_fail$lengths)
  pos_start <- pos_end - rle_fail$lengths + 1L

  k <- 0L
  for (i in seq_along(rle_fail$values)) {
    if (!rle_fail$values[i]) next
    k <- k + 1L

    idx_i <- pos_start[i]:pos_end[i]
    x_i <- st$sim_deficit[idx_i]  # positive deficit within the event

    event_values[k] <- switch(event_stat,
                              sum  = sum(x_i),
                              mean = mean(x_i),
                              max  = max(x_i)
    )
  }

  out_vulnerability <- switch(event_agg,
                              mean   = mean(event_values),
                              median = stats::median(event_values),
                              max    = max(event_values)
  )

  if (!return_components) return(out_vulnerability)

  list(
    vulnerability = out_vulnerability,
    n_events = as.integer(n_events),
    event_values = event_values,
    event_lengths = fail_event_lengths,
    n_dropped = st$n_dropped
  )
}


#' Extract Hashimoto binary state, deficit, events, and transitions
#'
#' Creates a standardized representation of Hashimoto-style system state from
#' threshold and supply series, suitable for computing reliability, resilience,
#' vulnerability, and event/run diagnostics.
#'
#' @details
#' Success is defined as `sim_supply >= obs_threshold` and failure as
#' `sim_supply < obs_threshold`. Deficit magnitude is defined as
#' `max(obs_threshold - sim_supply, 0)` (zero in success).
#'
#' NA handling:
#' - `na_action = "omit"` drops timesteps where either input is non-finite.
#' - `na_action = "fail"` errors if any non-finite values occur.
#'
#' Event/run logic:
#' Failure events are contiguous runs of failure timesteps (via `rle()`).
#'
#' Transition logic:
#' Transitions are computed on the **cleaned** series using timestep pairs
#' `t -> t+1`. `is_fail_t` and `is_success_tp1` are aligned to transitions.
#'
#' @param obs_threshold Numeric vector. Threshold per timestep (same units as supply).
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" (drop non-finite pairs) or "fail" (error).
#' @param include_events Logical. If TRUE, compute failure event diagnostics.
#' @param include_transitions Logical. If TRUE, compute transition-aligned vectors.
#'
#' @return A list with elements:
#' - `obs_threshold` numeric vector (cleaned)
#' - `sim_supply` numeric vector (cleaned)
#' - `sim_deficit` numeric vector (cleaned; >= 0)
#' - `is_success` logical vector
#' - `is_fail` logical vector
#' - `n_steps` integer, length of cleaned series
#' - `n_dropped` integer, number of dropped timesteps due to NA handling
#'
#' If `include_events = TRUE`, also:
#' - `n_fail_events` integer
#' - `fail_event_lengths` integer vector (run lengths)
#' - `fail_event_starts` integer vector (start indices, 1-based, cleaned series)
#' - `fail_event_ends` integer vector (end indices, 1-based, cleaned series)
#'
#' If `include_transitions = TRUE`, also:
#' - `n_transitions` integer (n_steps - 1 if n_steps >= 2 else 0)
#' - `is_fail_t` logical vector length n_transitions
#' - `is_success_tp1` logical vector length n_transitions
#'
#' @export
#'
#' @examples
#' obs_threshold <- rep(10, 6)
#' sim_supply <- c(12, 8, 7, 11, 9, 10)
#' st <- extract_state_hashimoto(obs_threshold, sim_supply)
#' mean(st$is_success)  # reliability
extract_state_hashimoto <- function(obs_threshold,
                                    sim_supply,
                                    na_action = c("omit", "fail"),
                                    include_events = TRUE,
                                    include_transitions = TRUE) {

  na_action <- match.arg(na_action)

  if (!is.numeric(obs_threshold) || !is.numeric(sim_supply)) {
    stop("`obs_threshold` and `sim_supply` must be numeric vectors.")
  }
  if (length(obs_threshold) != length(sim_supply)) {
    stop("`obs_threshold` and `sim_supply` must have the same length.")
  }

  n_in <- length(obs_threshold)
  if (n_in == 0L) {
    out <- list(
      obs_threshold = numeric(0),
      sim_supply = numeric(0),
      sim_deficit = numeric(0),
      is_success = logical(0),
      is_fail = logical(0),
      n_steps = 0L,
      n_dropped = 0L
    )
    if (include_events) {
      out$n_fail_events <- 0L
      out$fail_event_lengths <- integer(0)
      out$fail_event_starts <- integer(0)
      out$fail_event_ends <- integer(0)
    }
    if (include_transitions) {
      out$n_transitions <- 0L
      out$is_fail_t <- logical(0)
      out$is_success_tp1 <- logical(0)
    }
    return(out)
  }

  idx_ok <- is.finite(obs_threshold) & is.finite(sim_supply)
  n_dropped <- sum(!idx_ok)

  if (n_dropped > 0L && na_action == "fail") {
    stop("Non-finite values found in `obs_threshold` or `sim_supply`.")
  }

  if (na_action == "omit") {
    obs_threshold <- obs_threshold[idx_ok]
    sim_supply <- sim_supply[idx_ok]
  }

  is_success <- sim_supply >= obs_threshold
  is_fail <- !is_success
  sim_deficit <- pmax(obs_threshold - sim_supply, 0)

  out <- list(
    obs_threshold = obs_threshold,
    sim_supply = sim_supply,
    sim_deficit = sim_deficit,
    is_success = is_success,
    is_fail = is_fail,
    n_steps = as.integer(length(is_success)),
    n_dropped = as.integer(n_dropped)
  )

  # ---- events (failure runs) ------------------------------------------------
  if (include_events) {
    if (out$n_steps == 0L || !any(is_fail)) {
      out$n_fail_events <- 0L
      out$fail_event_lengths <- integer(0)
      out$fail_event_starts <- integer(0)
      out$fail_event_ends <- integer(0)
    } else {
      r <- rle(is_fail)
      ends <- cumsum(r$lengths)
      starts <- ends - r$lengths + 1L

      idx_fail_runs <- which(r$values)
      out$fail_event_lengths <- as.integer(r$lengths[idx_fail_runs])
      out$fail_event_starts <- as.integer(starts[idx_fail_runs])
      out$fail_event_ends <- as.integer(ends[idx_fail_runs])
      out$n_fail_events <- as.integer(length(idx_fail_runs))
    }
  }

  # ---- transitions (t -> t+1) ----------------------------------------------
  if (include_transitions) {
    if (out$n_steps < 2L) {
      out$n_transitions <- 0L
      out$is_fail_t <- logical(0)
      out$is_success_tp1 <- logical(0)
    } else {
      out$n_transitions <- as.integer(out$n_steps - 1L)
      out$is_fail_t <- out$is_fail[-out$n_steps]
      out$is_success_tp1 <- out$is_success[-1L]
    }
  }

  out
}

#' Failure diagnostics summary (Hashimoto state)
#'
#' Convenience wrapper that returns common failure diagnostics derived from the
#' Hashimoto binary state definition (success: supply >= threshold).
#'
#' @param obs_threshold Numeric vector. Threshold per timestep.
#' @param sim_supply Numeric vector. Supply per timestep.
#' @param na_action Character scalar: "omit" or "fail".
#' @param no_failure Character scalar: what to return for duration/severity when no events:
#'   "na" (default) or "zero".
#'
#' @return A list with:
#' - `n_fail_events` integer, number of failure events
#' - `n_fail_steps` integer, number of failure timesteps
#' - `fail_frequency` numeric, events per timestep
#' - `mean_fail_duration` numeric, mean event duration (timesteps)
#' - `mean_event_peak_deficit` numeric, mean across events of max deficit (units of supply)
#' @export
failure_metrics_hashimoto <- function(obs_threshold,
                                      sim_supply,
                                      na_action = c("omit", "fail"),
                                      no_failure = c("na", "zero")) {

  na_action <- match.arg(na_action)
  no_failure <- match.arg(no_failure)

  st <- extract_state_hashimoto(
    obs_threshold = obs_threshold,
    sim_supply = sim_supply,
    na_action = na_action,
    include_events = TRUE,
    include_transitions = FALSE
  )

  n_steps <- st$n_steps
  if (n_steps == 0L) {
    return(list(
      n_fail_events = 0L,
      n_fail_steps = 0L,
      fail_frequency = NA_real_,
      mean_fail_duration = NA_real_,
      mean_event_peak_deficit = NA_real_
    ))
  }

  n_fail_steps <- as.integer(sum(st$is_fail))
  n_fail_events <- if (!is.null(st$n_fail_events)) st$n_fail_events else 0L
  fail_frequency <- n_fail_events / n_steps

  if (n_fail_events == 0L) {
    out_empty <- if (no_failure == "na") NA_real_ else 0
    return(list(
      n_fail_events = n_fail_events,
      n_fail_steps = n_fail_steps,
      fail_frequency = fail_frequency,
      mean_fail_duration = out_empty,
      mean_event_peak_deficit = out_empty
    ))
  }

  mean_fail_duration <- mean(st$fail_event_lengths)

  # mean across events of max deficit within each event
  event_peaks <- numeric(n_fail_events)
  for (k in seq_len(n_fail_events)) {
    i0 <- st$fail_event_starts[k]
    i1 <- st$fail_event_ends[k]
    event_peaks[k] <- max(st$sim_deficit[i0:i1])
  }
  mean_event_peak_deficit <- mean(event_peaks)

  list(
    n_fail_events = as.integer(n_fail_events),
    n_fail_steps = n_fail_steps,
    fail_frequency = fail_frequency,
    mean_fail_duration = mean_fail_duration,
    mean_event_peak_deficit = mean_event_peak_deficit
  )
}
