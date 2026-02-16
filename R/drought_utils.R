#' Compute Maximum Dry-Spell Duration
#'
#' Calculates the longest continuous sequence of dry days in a precipitation
#' time series. A dry day is defined as any day with precipitation below a user-
#' specified threshold. The function uses run-length encoding to identify
#' consecutive dry runs and returns the maximum duration in days.
#'
#' @param P Numeric vector of daily precipitation values.
#' @param threshold Numeric. Precipitation threshold (in mm) used to define a
#'   dry day. Defaults to `1` (i.e., days with P < 1 mm are considered dry).
#'
#' @return
#' An integer value representing the length (in days) of the longest continuous
#' dry spell. Returns `0` if no dry days occur in the series.
#'
#' @examples
#' # Example precipitation series
#' P <- c(0, 0.2, 0.5, 3, 0, 0, 0, 5)
#'
#' # Longest dry spell with default threshold (1 mm)
#' max_dry_spell(P)
#'
#' # Using a stricter threshold
#' max_dry_spell(P, threshold = 0.1)
#'
#' @export
max_dry_spell <- function(P, threshold = 1) {

  if (!is.numeric(P)) stop("P must be a numeric vector.", call. = FALSE)

  n_na <- sum(is.na(P))
  if (n_na > 0L) {
    warning(
      "max_dry_spell: ", n_na, " NA value(s) removed before computing dry spells.",
      call. = FALSE
    )
    P <- P[!is.na(P)]
  }
  if (length(P) == 0L) return(0L)

  dry <- P < threshold
  r <- rle(dry)
  dry_lengths <- r$lengths[r$values == TRUE]

  if (length(dry_lengths) == 0) return(0L)
  max(dry_lengths)
}

#' Compute Drought Probability Index (DPI)
#'
#' Calculates the fraction of SPEI (or similar standardized index) values that
#' fall below a drought threshold. Higher values indicate a larger proportion
#' of the record spent in drought conditions.
#'
#' @param spei_values Numeric vector of standardized index values (e.g., SPEI, SPI).
#' @param threshold Numeric scalar. Values strictly below this threshold are
#'   classified as drought. Default \code{-1} (moderate drought on the SPEI
#'   scale).
#'
#' @return Numeric scalar between 0 and 1 representing the fraction of
#'   non-\code{NA} values below \code{threshold}, or \code{0} if no
#'   finite values are available.
#'
#' @examples
#' spei_vals <- c(-1.5, -0.3, 0.8, -1.2, NA, 0.1)
#' compute_dpi(spei_vals)
#'
#' @export
compute_dpi <- function(spei_values, threshold = -1) {
  x <- spei_values[!is.na(spei_values)]
  if (length(x) == 0L) return(0)
  mean(x < threshold)
}


#' Calculate the Standardized Precipitation Evapotranspiration Index (SPEI)
#'
#' @description
#' Computes the Standardized Precipitation Evapotranspiration Index (SPEI)
#' from monthly precipitation and potential evapotranspiration (PET) data,
#' using the statistical framework implemented in the \pkg{SPEI} package.
#'
#' The function first computes the climatic water balance \eqn{D = P - PET},
#' converts it into a monthly time series, and applies a probability
#' distribution (by default, log-logistic) to obtain standardized
#' anomalies of moisture deficit or surplus at a chosen accumulation scale.
#'
#' @param P Numeric vector of monthly precipitation values (mm).
#' @param PET Numeric vector of monthly potential evapotranspiration values (mm).
#' @param scale Integer. Accumulation period in months (e.g., 1, 3, 6, 12).
#'   This defines the temporal scale over which the water balance is aggregated
#'   before standardization.
#' @param dist Character string indicating the distribution to fit.
#'   Common choices are `"log-Logistic"` (default) or `"Gamma"`.
#'   See \link[SPEI:spei]{SPEI::spei()} for available options.
#' @param ref_start Optional numeric vector of length two (e.g., \code{c(1981, 1)})
#'   specifying the start year and month of the reference period used for
#'   fitting the distribution. If \code{NULL}, the entire record is used.
#' @param ref_end Optional numeric vector of length two (e.g., \code{c(2010, 12)})
#'   specifying the end year and month of the reference period used for
#'   fitting the distribution.
#' @param dates Optional vector of class \code{Date} corresponding to each
#'   observation. If provided, the function automatically determines the
#'   start year and month for the internal time-series object. If omitted,
#'   the series is assumed to begin at (1, 1) with monthly frequency.
#'
#' @details
#' The function acts as a simplified wrapper around \code{\link[SPEI:spei]{SPEI::spei()}}.
#' It is designed for convenience in workflows where precipitation (P),
#' evapotranspiration (PET), and dates are stored as plain vectors rather
#' than time-series objects. The resulting SPEI values are standardized
#' z-scores representing the relative moisture deficit or surplus at each time step.
#'
#' Positive SPEI values indicate wetter-than-normal conditions,
#' while negative values indicate drier-than-normal (potential drought)
#' conditions.
#'
#' @return
#' A numeric vector of SPEI values (same length as input),
#' corresponding to the fitted index at each time step. If the underlying
#' distribution fit emitted warnings, they are stored in the
#' \code{"fit_warnings"} attribute (accessible via
#' \code{attr(result, "fit_warnings")}). A user-facing warning is issued
#' whenever the output contains \code{NaN} values.
#'
#' @references
#' BeguerÃ­a, S., Vicente-Serrano, S. M., Reig, F., & Latorre, B. (2014).
#' Standardized precipitation evapotranspiration index (SPEI) revisited:
#' parameter fitting, evapotranspiration models, tools, datasets and trends worldwide.
#' \emph{International Journal of Climatology, 34}(10), 3001â€“3023.
#'
#' Vicente-Serrano, S. M., BeguerÃ­a, S., & LÃ³pez-Moreno, J. I. (2010).
#' A multiscalar drought index sensitive to global warming:
#' The Standardized Precipitation Evapotranspiration Index.
#' \emph{Journal of Climate, 23}(7), 1696â€“1718.
#'
#' @seealso
#' \code{\link{calculate_spi}} for the precipitation-only variant (SPI),
#' \code{\link[SPEI:spei]{SPEI::spei()}} for the underlying implementation,
#' and \code{\link[SPEI:spi]{SPEI::spi()}} for the SPEI package's SPI function.
#'
#' @examples
#' \dontrun{
#' library(SPEI)
#' set.seed(42)
#' dates <- seq(as.Date("1981-01-01"), as.Date("2020-12-01"), by = "month")
#' P   <- rnorm(length(dates), 60, 15)  # precipitation (mm)
#' PET <- rnorm(length(dates), 45, 10)  # PET (mm)
#'
#' spei6 <- calculate_spei(P, PET, scale = 6, dates = dates,
#'                    ref_start = c(1981, 1), ref_end = c(2010, 12))
#'
#' plot(dates, spei6, type = "l", col = "steelblue",
#'      ylab = "SPEI (6-month)", xlab = "Year")
#' abline(h = c(-2, -1, 0, 1, 2), lty = 2, col = "gray")
#' }
#'
#' @export
calculate_spei <- function(P, PET, scale = 3,
                           dist = "log-Logistic",
                           ref_start = NULL,
                           ref_end = NULL,
                           dates = NULL) {
  if (!requireNamespace("SPEI", quietly = TRUE))
    stop("Package 'SPEI' is required. Install with install.packages('SPEI').")

  stopifnot(length(P) == length(PET))

  D <- P - PET

  if (!is.null(dates)) {
    start_year  <- as.numeric(format(min(dates), "%Y"))
    start_month <- as.numeric(format(min(dates), "%m"))
    D <- ts(D, start = c(start_year, start_month), frequency = 12)
  } else {
    D <- ts(D, frequency = 12)
  }

  # Capture warnings from the SPEI fit rather than suppressing them entirely.
  # The SPEI package can produce console output via cat(); capture.output()
  # absorbs that. Genuine warnings are collected and re-surfaced below.
  fit_warnings <- character(0)
  spei_obj <- withCallingHandlers(
    suppressMessages({
      tmp <- capture.output(
        res <- SPEI::spei(D,
                          scale = scale,
                          distribution = dist,
                          ref.start = ref_start,
                          ref.end = ref_end),
        file = NULL
      )
      res
    }),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  out <- as.numeric(spei_obj$fitted)

  # Attach captured warnings as an attribute for programmatic inspection
  if (length(fit_warnings) > 0L) {
    attr(out, "fit_warnings") <- fit_warnings
  }

  # Surface a single user-facing warning when the fit produced NaN values
  n_nan <- sum(is.nan(out))
  if (n_nan > 0L) {
    warning(
      "SPEI fit produced ", n_nan, " NaN value(s) out of ", length(out),
      " time steps. Consider a different distribution, a longer series, or ",
      "check for zero-variance periods in the water balance.",
      call. = FALSE
    )
  }

  out
}


#' Calculate the Standardized Precipitation Index (SPI)
#'
#' @description
#' Computes the Standardized Precipitation Index (SPI) from monthly
#' precipitation data, using the statistical framework implemented in the
#' \pkg{SPEI} package. Unlike SPEI, SPI uses only precipitation (no PET)
#' and fits a gamma (or other) distribution to the accumulated precipitation
#' at the chosen time scale.
#'
#' @param P Numeric vector of monthly precipitation values (mm).
#' @param scale Integer. Accumulation period in months (e.g., 1, 3, 6, 12).
#' @param dist Character string indicating the distribution to fit.
#'   Default \code{"Gamma"}. See \code{\link[SPEI:spi]{SPEI::spi()}} for
#'   available options.
#' @param ref_start Optional numeric vector of length two (e.g., \code{c(1981, 1)})
#'   specifying the start of the reference period for fitting.
#' @param ref_end Optional numeric vector of length two (e.g., \code{c(2010, 12)})
#'   specifying the end of the reference period.
#' @param dates Optional vector of class \code{Date} corresponding to each
#'   observation. If provided, the start year and month are inferred
#'   automatically. If omitted, the series begins at \code{c(1, 1)}.
#'
#' @details
#' This is a convenience wrapper around \code{\link[SPEI:spi]{SPEI::spi()}}
#' analogous to \code{\link{calculate_spei}}. The SPI is widely used in
#' operational drought monitoring and requires only precipitation as input,
#' making it applicable when evapotranspiration data are unavailable.
#'
#' Positive values indicate wetter-than-normal conditions; negative values
#' indicate drier-than-normal conditions.
#'
#' @return
#' A numeric vector of SPI values (same length as input). If the distribution
#' fit emitted warnings, they are stored in the \code{"fit_warnings"} attribute.
#' A user-facing warning is issued whenever the output contains \code{NaN}
#' values.
#'
#' @references
#' McKee, T. B., Doesken, N. J., & Kleist, J. (1993).
#' The relationship of drought frequency and duration to time scales.
#' \emph{Proceedings of the 8th Conference on Applied Climatology}, 179--184.
#'
#' @seealso
#' \code{\link{calculate_spei}} for the SPEI variant (requires PET),
#' \code{\link[SPEI:spi]{SPEI::spi()}} for the underlying implementation.
#'
#' @examples
#' \dontrun{
#' library(SPEI)
#' set.seed(42)
#' dates <- seq(as.Date("1981-01-01"), as.Date("2020-12-01"), by = "month")
#' P <- rgamma(length(dates), shape = 2, rate = 0.03)
#'
#' spi3 <- calculate_spi(P, scale = 3, dates = dates,
#'                        ref_start = c(1981, 1), ref_end = c(2010, 12))
#'
#' plot(dates, spi3, type = "l", col = "steelblue",
#'      ylab = "SPI (3-month)", xlab = "Year")
#' abline(h = c(-2, -1, 0, 1, 2), lty = 2, col = "gray")
#' }
#'
#' @export
calculate_spi <- function(P, scale = 3,
                          dist = "Gamma",
                          ref_start = NULL,
                          ref_end = NULL,
                          dates = NULL) {
  if (!requireNamespace("SPEI", quietly = TRUE))
    stop("Package 'SPEI' is required. Install with install.packages('SPEI').")

  if (!is.numeric(P))
    stop("P must be a numeric vector.", call. = FALSE)

  Pts <- if (!is.null(dates)) {
    start_year  <- as.numeric(format(min(dates), "%Y"))
    start_month <- as.numeric(format(min(dates), "%m"))
    ts(P, start = c(start_year, start_month), frequency = 12)
  } else {
    ts(P, frequency = 12)
  }

  fit_warnings <- character(0)
  spi_obj <- withCallingHandlers(
    suppressMessages({
      tmp <- capture.output(
        res <- SPEI::spi(Pts,
                         scale = scale,
                         distribution = dist,
                         ref.start = ref_start,
                         ref.end = ref_end),
        file = NULL
      )
      res
    }),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  out <- as.numeric(spi_obj$fitted)

  if (length(fit_warnings) > 0L) {
    attr(out, "fit_warnings") <- fit_warnings
  }

  n_nan <- sum(is.nan(out))
  if (n_nan > 0L) {
    warning(
      "SPI fit produced ", n_nan, " NaN value(s) out of ", length(out),
      " time steps. Consider a different distribution or a longer series.",
      call. = FALSE
    )
  }

  out
}
