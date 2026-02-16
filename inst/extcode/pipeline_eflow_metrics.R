# ==============================================================================
# eflow_metrics_pipeline.R
# Comprehensive demonstration of calculate_hydro_metrics() capabilities
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

devtools::load_all()

# ------------------------------------------------------------------------------
# 0) Validate registry invariants (development-time check)
# ------------------------------------------------------------------------------
.hydro_validate_registry(strict = TRUE)
cat("Registry validation passed.\n\n")

# ==============================================================================
# SECTION 1: Data generation — realistic synthetic discharge
# ==============================================================================
# We generate two discharge regimes to use throughout:
#   - A perennial river with strong seasonality (wet winters, dry summers)
#   - An intermittent stream with frequent zero-flow periods

set.seed(42)
date <- seq(as.Date("1990-01-01"), as.Date("2020-12-31"), by = "day")
n    <- length(date)
doy  <- as.integer(format(date, "%j"))
yr   <- as.integer(format(date, "%Y"))

# Perennial river: baseflow ~15 m³/s, winter peak ~50, summer trough ~5
seasonal <- 15 + 20 * cos(2 * pi * (doy - 30) / 365)   # peak ~Jan
noise    <- rgamma(n, shape = 3, rate = 0.8)
Q_perennial <- pmax(seasonal + noise, 0.1)

# Intermittent stream: low baseflow, many zeros in summer
Q_intermittent <- rgamma(n, shape = 0.8, rate = 0.3) *
  (1 + 0.6 * cos(2 * pi * (doy - 30) / 365))
Q_intermittent[Q_intermittent < 0.5 & runif(n) > 0.4] <- 0

cat("Data generated:", n, "days,", length(unique(yr)), "years\n\n")

# ==============================================================================
# SECTION 2: Default ERFA70 metric set — full canonical workflow
# ==============================================================================
# The default call uses:
#   metric_set = "erfa70"   → all 35 base metrics
#   summaries  = c("med", "iqr") → 70 summary rows (35 × 2)
#   water_year_start = 10   → Oct-Sep water years (standard IHA practice)

erfa70_result <- calculate_hydro_metrics(
  Q    = Q_perennial,
  date = date
)

cat("--- Default ERFA70 output ---\n")
cat("Rows:", nrow(erfa70_result), "(expected: 70)\n")
cat("Columns:", paste(names(erfa70_result), collapse = ", "), "\n")
cat("metric_id range:", range(erfa70_result$metric_id, na.rm = TRUE), "\n\n")

# Peek at one metric per group
erfa70_result %>%
  filter(stat == "med") %>%
  group_by(group) %>%
  slice_head(n = 1) %>%
  select(group, metric_key, metric, value) %>%
  print()

cat("\n")

# ==============================================================================
# SECTION 3: Water year vs calendar year comparison
# ==============================================================================
# Water year = 10 (Oct start) avoids splitting the Northern Hemisphere wet
# season across year boundaries. Calendar year (water_year_start = 1) can
# introduce edge-of-year rolling-mean artefacts for long windows (90-day).

wy_oct <- calculate_hydro_metrics(
  Q_perennial, date,
  water_year_start = 10L,
  metrics = c("amax90d", "amin90d"),
  summaries = "med"
)

wy_cal <- calculate_hydro_metrics(
  Q_perennial, date,
  water_year_start = 1L,
  metrics = c("amax90d", "amin90d"),
  summaries = "med"
)

cat("--- Water year vs calendar year (90-day extremes, median) ---\n")
comparison <- bind_rows(
  wy_oct %>% mutate(wy = "Oct (WY)"),
  wy_cal %>% mutate(wy = "Jan (CY)")
) %>%
  select(wy, metric_key, value) %>%
  pivot_wider(names_from = wy, values_from = value)

print(comparison)
cat("\n")

# ==============================================================================
# SECTION 4: Threshold scope options
# ==============================================================================
# Pulse and zero-flow detection depends on how thresholds are estimated.
# Four scopes are available: global, annual, seasonal, baseline.

pulse_metrics <- c("num_high_pulses", "num_low_pulses",
                   "avg_dur_high_pulses", "avg_dur_low_pulses")

# 4a) Global: thresholds from the entire record
m_global <- calculate_hydro_metrics(
  Q_perennial, date,
  threshold_scope = "global",
  metrics = pulse_metrics, summaries = "med"
)

# 4b) Annual: thresholds recomputed per water year
m_annual <- calculate_hydro_metrics(
  Q_perennial, date,
  threshold_scope = "annual",
  metrics = pulse_metrics, summaries = "med"
)

# 4c) Seasonal: thresholds by calendar month across all years
m_seasonal <- calculate_hydro_metrics(
  Q_perennial, date,
  threshold_scope = "seasonal",
  metrics = pulse_metrics, summaries = "med"
)

# 4d) Baseline: thresholds from a reference period (1991–2010)
m_baseline <- calculate_hydro_metrics(
  Q_perennial, date,
  threshold_scope = "baseline",
  baseline_years = 1991:2010,
  metrics = pulse_metrics, summaries = "med"
)

cat("--- Threshold scope comparison (pulse metrics, median) ---\n")
scope_comp <- bind_rows(
  m_global   %>% mutate(scope = "global"),
  m_annual   %>% mutate(scope = "annual"),
  m_seasonal %>% mutate(scope = "seasonal"),
  m_baseline %>% mutate(scope = "baseline")
) %>%
  select(scope, metric_key, value) %>%
  pivot_wider(names_from = scope, values_from = value)

print(scope_comp)
cat("\n")

# ==============================================================================
# SECTION 5: Baseline date range (alternative to baseline_years)
# ==============================================================================
m_baseline_dates <- calculate_hydro_metrics(
  Q_perennial, date,
  threshold_scope = "baseline",
  baseline_dates = c(as.Date("1995-04-01"), as.Date("2005-09-30")),
  metrics = c("num_high_pulses", "num_low_pulses"),
  summaries = "med"
)

cat("--- Baseline via date range (1995-04 to 2005-09) ---\n")
print(m_baseline_dates %>% select(metric_key, value))
cat("\n")

# ==============================================================================
# SECTION 6: Zero-flow threshold modes
# ==============================================================================
# Three modes for defining "zero flow":
#   absolute        → Q <= zero_thr (default 0.02)
#   quantile        → Q <= quantile(Q, zero_q)
#   fraction_median → Q <= zero_frac_median * median(Q)

zero_metrics <- c("num_zero_seq", "avg_dur_zero_seq", "time_main_zero_seq")

z_abs <- calculate_hydro_metrics(
  Q_intermittent, date,
  zero_thr_mode = "absolute", zero_thr = 0.01,
  metrics = zero_metrics, summaries = "med"
)

z_quant <- calculate_hydro_metrics(
  Q_intermittent, date,
  zero_thr_mode = "quantile", zero_q = 0.05,
  metrics = zero_metrics, summaries = "med"
)

z_frac <- calculate_hydro_metrics(
  Q_intermittent, date,
  zero_thr_mode = "fraction_median", zero_frac_median = 0.01,
  metrics = zero_metrics, summaries = "med"
)

cat("--- Zero-flow threshold mode comparison (intermittent stream) ---\n")
zero_comp <- bind_rows(
  z_abs   %>% mutate(mode = "absolute (0.01)"),
  z_quant %>% mutate(mode = "quantile (5th)"),
  z_frac  %>% mutate(mode = "frac_median (1%)")
) %>%
  select(mode, metric_key, value) %>%
  pivot_wider(names_from = mode, values_from = value)

print(zero_comp)
cat("\n")

# ==============================================================================
# SECTION 7: Rate-of-change tuning (roc_eps and roc_smooth_k)
# ==============================================================================
# roc_eps:      deadband — daily changes <= roc_eps are ignored (reduces noise)
# roc_smooth_k: centered moving average window before differencing

roc_metrics <- c("mean_rate_rise", "mean_rate_fall", "num_rises", "num_falls")

roc_default <- calculate_hydro_metrics(
  Q_perennial, date,
  roc_eps = 0, roc_smooth_k = 1L,
  metrics = roc_metrics, summaries = "med"
)

roc_smoothed <- calculate_hydro_metrics(
  Q_perennial, date,
  roc_eps = 0.5, roc_smooth_k = 5L,
  metrics = roc_metrics, summaries = "med"
)

cat("--- Rate-of-change: raw vs smoothed (k=5, eps=0.5) ---\n")
roc_comp <- bind_rows(
  roc_default  %>% mutate(config = "raw (k=1, eps=0)"),
  roc_smoothed %>% mutate(config = "smooth (k=5, eps=0.5)")
) %>%
  select(config, metric_key, value) %>%
  pivot_wider(names_from = config, values_from = value)

print(roc_comp)
cat("\n")

# ==============================================================================
# SECTION 8: Monthly completeness enforcement (month_min_frac)
# ==============================================================================
# Inject gaps into a copy and show how month_min_frac affects monthly means

Q_gappy <- Q_perennial
# Remove all of July 2005 except 5 days → ~16% coverage
jul05 <- which(format(date, "%Y-%m") == "2005-07")
Q_gappy[jul05[6:length(jul05)]] <- NA

m_strict <- calculate_hydro_metrics(
  Q_gappy, date,
  month_min_frac = 0.8,
  metrics = c("july_flow"), summaries = "med"
)

m_lax <- calculate_hydro_metrics(
  Q_gappy, date,
  month_min_frac = 0.0,
  metrics = c("july_flow"), summaries = "med"
)

cat("--- Monthly completeness: strict (80%) vs lax (0%) ---\n")
cat("  july_flow median (strict):", m_strict$value, "\n")
cat("  july_flow median (lax):   ", m_lax$value, "\n\n")

# ==============================================================================
# SECTION 9: Purpose-based metric selection
# ==============================================================================
# Metrics are tagged with purpose labels: hydrology, eflow, erfa, navigation,
# energy, irrigation. When metric_set = NULL, purpose selects the subset.

purposes <- c("navigation", "energy", "irrigation")

for (p in purposes) {
  m <- calculate_hydro_metrics(
    Q_perennial, date,
    metric_set = NULL, purpose = p,
    summaries = "med"
  )
  cat(sprintf("  Purpose %-12s → %2d metrics: %s ...\n",
              paste0("'", p, "'"), nrow(m),
              paste(head(m$metric_key, 4), collapse = ", ")))
}
cat("\n")

# ==============================================================================
# SECTION 10: Custom summary functions
# ==============================================================================
# Instead of "med"/"iqr", pass a named list of functions

custom_summaries <- list(
  p10 = function(x) quantile(x, 0.10, na.rm = TRUE),
  p50 = function(x) quantile(x, 0.50, na.rm = TRUE),
  p90 = function(x) quantile(x, 0.90, na.rm = TRUE)
)

m_custom <- calculate_hydro_metrics(
  Q_perennial, date,
  metrics = c("amax1d", "amin7d", "mean_rate_rise"),
  summaries = custom_summaries
)

cat("--- Custom summaries (10th / 50th / 90th percentile) ---\n")
m_custom %>%
  select(metric_key, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print()
cat("\n")

# ==============================================================================
# SECTION 11: Multi-site analysis
# ==============================================================================
Q_mat <- cbind(
  perennial    = Q_perennial,
  intermittent = Q_intermittent
)

multi_out <- calculate_hydro_metrics(
  Q_mat, date,
  metrics = c("amax1d", "amin1d", "num_zero_seq", "mean_rate_rise"),
  summaries = "med"
)

cat("--- Multi-site results ---\n")
multi_out %>%
  select(site, metric_key, value) %>%
  pivot_wider(names_from = site, values_from = value) %>%
  print()
cat("\n")

# ==============================================================================
# SECTION 12: Annual diagnostic table (return_annual = TRUE)
# ==============================================================================
diag <- calculate_hydro_metrics(
  Q_perennial, date,
  metrics = c("amax1d", "amin1d", "january_flow", "july_flow"),
  summaries = c("med", "iqr"),
  return_annual = TRUE
)

cat("--- Annual table (first 3 water years × 4 metrics) ---\n")
diag$annual %>%
  filter(year <= min(year) + 2) %>%
  pivot_wider(names_from = metric_key, values_from = value) %>%
  print()

cat("\n--- Summary table ---\n")
diag$summary %>%
  select(metric_key, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print()
cat("\n")

# ==============================================================================
# SECTION 13: Multi-site + return_annual
# ==============================================================================
multi_diag <- calculate_hydro_metrics(
  Q_mat, date,
  metrics = c("amax1d", "amin1d"),
  summaries = "med",
  return_annual = TRUE
)

cat("--- Multi-site annual table (first 2 years) ---\n")
multi_diag$annual %>%
  filter(year <= min(year) + 1) %>%
  pivot_wider(names_from = metric_key, values_from = value) %>%
  print()
cat("\n")

# ==============================================================================
# SECTION 14: Threshold quantile sensitivity
# ==============================================================================
# Varying high_thresh_quantile shows how pulse counts change

cat("--- High-pulse count sensitivity to threshold quantile ---\n")
for (hq in c(0.75, 0.85, 0.90, 0.95)) {
  m <- calculate_hydro_metrics(
    Q_perennial, date,
    high_thresh_quantile = hq,
    metrics = c("num_high_pulses", "avg_dur_high_pulses"),
    summaries = "med"
  )
  vals <- setNames(m$value, m$metric_key)
  cat(sprintf("  Q%.0f threshold → pulses: %.1f, avg duration: %.1f days\n",
              hq * 100, vals["num_high_pulses"], vals["avg_dur_high_pulses"]))
}
cat("\n")

# ==============================================================================
# SECTION 15: Full ERFA70 multi-site with annual output for downstream ERFA
# ==============================================================================
# This is the typical end-to-end setup before passing into evaluate_erfa_class()

cat("--- Full ERFA70 multi-site computation ---\n")

erfa_base <- calculate_hydro_metrics(
  Q_mat, date,
  metric_set = "erfa70",
  summaries  = c("med", "iqr"),
  threshold_scope = "baseline",
  baseline_years  = 1991:2010,
  return_annual   = TRUE
)

cat("Summary rows:", nrow(erfa_base$summary), "\n")
cat("Annual rows: ", nrow(erfa_base$annual), "\n")
cat("Sites:       ", paste(unique(erfa_base$summary$site), collapse = ", "), "\n")

erfa_base$summary %>%
  filter(stat == "med") %>%
  group_by(site, group) %>%
  summarise(n = n(), mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  print()

cat("\nPipeline complete.\n")
