# ------------------------------------------------------------------------------
# Internal validation helpers
# ------------------------------------------------------------------------------

#' Check if value is a scalar integer
#' @param x Value to check.
#' @return Logical.
#' @keywords internal
.is_int_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x == as.integer(x)
}

#' Check if value is a scalar numeric
#' @param x Value to check.
#' @return Logical.
#' @keywords internal
.is_num_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

#' Check if value is a non-empty character scalar
#' @param x Value to check.
#' @return Logical.
#' @keywords internal
.is_chr_scalar <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' Validate numeric vector input
#' @param x Value to check.
#' @param name Argument name for error messages.
#' @param allow_na Allow NA values?
#' @return Invisible TRUE or stops with error.
#' @keywords internal
.validate_numeric_vector <- function(x, name = "x", allow_na = TRUE) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric.", name), call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(sprintf("'%s' contains NA values.", name), call. = FALSE)
  }
  invisible(TRUE)
}
