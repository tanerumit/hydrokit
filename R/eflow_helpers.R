# ==============================================================================
# Script: R/hydro_helpers.R
# Functions under test: .hydro_as_date(), .hydro_as_numeric(), .hydro_safe_max(),
# .hydro_safe_min(), .hydro_safe_which_max(), .hydro_safe_which_min(),
# .hydro_safe_mean(), .hydro_seq_stats()
# Purpose: Shared internal helpers for input coercion and safe summaries.
# ==============================================================================

#' Validate and coerce a Date vector
#'
#' @param x Vector convertible to Date.
#' @param name Character scalar. Argument name for error messages.
#'
#' @return Date vector.
#' @keywords internal
.hydro_as_date <- function(x, name = "date") {
  x <- as.Date(x)
  if (anyNA(x)) {
    stop(name, " contains NA after coercion to Date.", call. = FALSE)
  }
  x
}

#' Validate and coerce a numeric vector
#'
#' @param x Vector convertible to numeric.
#' @param name Character scalar. Argument name for error messages.
#'
#' @return Numeric vector.
#' @keywords internal
.hydro_as_numeric <- function(x, name = "Q") {
  x_num <- suppressWarnings(as.numeric(x))
  if (all(is.na(x_num))) {
    stop(name, " could not be coerced to numeric.", call. = FALSE)
  }
  x_num
}

#' Return the maximum of a numeric vector, or NA_real_ if all values are NA.
#' @param x Numeric vector.
#' @return Numeric scalar or NA_real_.
#' @keywords internal
.hydro_safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

#' Return the minimum of a numeric vector, or NA_real_ if all values are NA.
#' @param x Numeric vector.
#' @return Numeric scalar or NA_real_.
#' @keywords internal
.hydro_safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

#' Return the index of the maximum value, or NA_integer_ if all values are NA.
#' @param x Numeric vector.
#' @return Integer index or NA_integer_.
#' @keywords internal
.hydro_safe_which_max <- function(x) {
  if (all(is.na(x))) return(NA_integer_)
  which.max(x)
}

#' Return the index of the minimum value, or NA_integer_ if all values are NA.
#' @param x Numeric vector.
#' @return Integer index or NA_integer_.
#' @keywords internal
.hydro_safe_which_min <- function(x) {
  if (all(is.na(x))) return(NA_integer_)
  which.min(x)
}

#' Return the mean of finite values, or NA_real_ if none are finite.
#' @param x Numeric vector.
#' @return Numeric scalar or NA_real_.
#' @keywords internal
.hydro_safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  mean(x)
}

#' Run-length summary statistics for logical sequences
#'
#' @param cond Logical vector.
#'
#' @return List with elements:
#'   - n_runs: integer number of TRUE runs
#'   - mean_len: numeric mean length of TRUE runs (0 if none)
#'   - start_idx_longest: integer start index (within cond) of the longest TRUE run
#'     (NA if none)
#' @keywords internal
.hydro_seq_stats <- function(cond) {
  cond <- as.logical(cond)
  r <- rle(cond)
  if (!any(r$values, na.rm = TRUE)) {
    return(list(n_runs = 0L, mean_len = 0, start_idx_longest = NA_integer_))
  }

  true_idx <- which(r$values)
  n_runs <- length(true_idx)
  true_lens <- r$lengths[true_idx]
  mean_len <- mean(true_lens)

  # longest TRUE run start index
  longest_run_i <- true_idx[which.max(true_lens)]
  start_idx <- sum(r$lengths[seq_len(longest_run_i - 1L)]) + 1L

  list(n_runs = n_runs, mean_len = mean_len, start_idx_longest = start_idx)
}
