



library(dplyr)

devtools::load_all()

# ------------------------------------------------------------------------------
# 0) (Optional) validate registry invariants during development
# ------------------------------------------------------------------------------
.hydro_validate_registry(strict = TRUE)

# ------------------------------------------------------------------------------
# 1) Prepare daily discharge series (single-site)
# ------------------------------------------------------------------------------
set.seed(1)
date <- seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "day")

# Example baseline discharge (gamma-ish, with seasonality)
doy <- as.integer(format(date, "%j"))
Q_base <- rgamma(length(date), shape = 2, rate = 0.15) * (1 + 0.3 * sin(2 * pi * doy / 365))

# Example scenario discharge (e.g., slightly drier + more zeros)
Q_scen <- Q_base * 0.9
Q_scen[sample.int(length(Q_scen), size = 80)] <- 0

# ------------------------------------------------------------------------------
# 2) Compute ERFA metrics (same 70 as before)
#    - metric_set="erfa70" gives the canonical ERFA subset
#    - summaries=c("med","iqr") yields the 70 rows per site (35 base x 2 stats)
# ------------------------------------------------------------------------------
m_base <- calculate_hydro_metrics(
  Q_base, date,
  metric_set = "erfa70",
  summaries = c("med", "iqr")
)

m_scen <- calculate_hydro_metrics(
  Q_scen, date,
  metric_set = "erfa70",
  summaries = c("med", "iqr")
)

# Inspect structure
m_base %>% select(metric_id, metric_key, stat, metric, group, value) %>% head(10)

# ------------------------------------------------------------------------------
# 3) ERFA classification (unchanged logic; compares chosen summary stat)
# ------------------------------------------------------------------------------
erfa <- evaluate_erfa_class(
  baseline_tbl = m_base,
  scenario_tbl = m_scen,
  sensi_threshold = 30,
  stat = "med",
  metric_set = "erfa70"
)

# ERFA summary table (group-level + overall)
erfa$summary

# Metric-level deltas (what triggered risk)
erfa$detail %>%
  filter(outside_range) %>%
  arrange(desc(abs(pct_change))) %>%
  select(metric_key, group, value_base, value_scen, pct_change) %>%
  head(15)

# ------------------------------------------------------------------------------
# 4) Purpose-based selection (new capability)
#    This selects all metrics tagged for a purpose.
#    Important: purpose is only used when metric_set = NULL and metrics = NULL.
# ------------------------------------------------------------------------------
nav_metrics <- calculate_hydro_metrics(
  Q_base, date,
  metric_set = NULL,
  purpose = "navigation",
  summaries = "med"
)

nav_metrics %>% count(group)

# ------------------------------------------------------------------------------
# 5) Explicit custom metric subset (overrides set/purpose)
# ------------------------------------------------------------------------------
custom <- calculate_hydro_metrics(
  Q_base, date,
  metrics = c("amin1d", "num_zero_seq", "mean_rate_fall"),
  summaries = c("med", "iqr")
)

custom

# ------------------------------------------------------------------------------
# 6) Multi-site workflow (matrix/data.frame with sites as columns)
# ------------------------------------------------------------------------------
Q_mat <- cbind(
  siteA = Q_base,
  siteB = Q_scen
)

multi_erfa <- calculate_hydro_metrics(
  Q_mat, date,
  metric_set = "erfa70",
  summaries = c("med", "iqr")
)

multi_erfa %>% select(site, metric_id, metric_key, stat, value) %>% head(10)

# ERFA per-site: split and evaluate
sites <- unique(multi_erfa$site)

erfa_by_site <- lapply(sites, function(s) {
  base_s <- multi_erfa %>% filter(site == s)
  # Here "siteA" is baseline-like and "siteB" scenario-like in our toy example,
  # so for real work you'd compute separate baseline/scenario per site.
  base_s
})

# ------------------------------------------------------------------------------
# 7) Annual table output (diagnostics)
# ------------------------------------------------------------------------------
diag <- calculate_hydro_metrics(
  Q_base, date,
  metric_set = "erfa70",
  summaries = c("med", "iqr"),
  return_annual = TRUE
)

# Summary table (same as default return)
diag$summary %>% head(8)

# Annual base metric values (long format: year x metric_key)
diag$annual %>% head(12)


