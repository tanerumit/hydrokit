# ==============================================================================
# erfa_pipeline.R
# Comprehensive demonstration of evaluate_erfa_class() capabilities
# ==============================================================================

library(dplyr)
library(tidyr)

devtools::load_all()

# ==============================================================================
# SECTION 1: Synthetic discharge data — baseline & scenario generation
# ==============================================================================
# We create a realistic setup: a baseline period and multiple climate/management
# scenarios applied to two river sites.

set.seed(123)
date <- seq(as.Date("1990-01-01"), as.Date("2020-12-31"), by = "day")
n    <- length(date)
doy  <- as.integer(format(date, "%j"))

# --- Site A: Perennial snowmelt-dominated river ---
seasonal_a <- 25 + 30 * cos(2 * pi * (doy - 90) / 365)  # spring peak
Q_base_a   <- pmax(seasonal_a + rgamma(n, 3, 0.5), 0.5)

# --- Site B: Rain-fed lowland river ---
seasonal_b <- 10 + 8 * cos(2 * pi * (doy - 30) / 365)   # winter peak
Q_base_b   <- pmax(seasonal_b + rgamma(n, 2, 0.6), 0.2)

# --- Scenario 1: Moderate abstraction (−15% flow, some low-flow stress) ---
Q_scen1_a <- Q_base_a * 0.85
Q_scen1_b <- Q_base_b * 0.85

# --- Scenario 2: Severe abstraction + flow regulation (−30%, altered timing) ---
Q_scen2_a <- Q_base_a * 0.70 + 2  # reduced peaks, raised baseflow
Q_scen2_b <- Q_base_b * 0.65
Q_scen2_b[Q_scen2_b < 0.3] <- 0   # intermittency introduced

# --- Scenario 3: Climate change (shifted seasonality, more extremes) ---
shift_doy  <- (doy + 15) %% 365 + 1   # 15-day shift in peak timing
seasonal_shift <- 25 + 35 * cos(2 * pi * (shift_doy - 90) / 365)
Q_scen3_a <- pmax(seasonal_shift + rgamma(n, 4, 0.4), 0.3)
Q_scen3_b <- Q_base_b * (0.9 + 0.2 * sin(2 * pi * doy / 365))

cat("Data generated:", n, "days for 2 sites × 4 conditions\n\n")

# ==============================================================================
# SECTION 2: Compute ERFA70 metrics for all conditions
# ==============================================================================
# calculate_hydro_metrics() with default settings produces the 70-row table
# (35 base metrics × {median, IQR}) that evaluate_erfa_class() expects.

compute_erfa70 <- function(Q, date, label = "") {
  out <- calculate_hydro_metrics(Q, date, metric_set = "erfa70", summaries = c("med", "iqr"))
  cat(sprintf("  %-30s → %d rows\n", label, nrow(out)))
  out
}

cat("--- Computing ERFA70 metrics ---\n")
m_base_a  <- compute_erfa70(Q_base_a,  date, "Baseline – Site A")
m_base_b  <- compute_erfa70(Q_base_b,  date, "Baseline – Site B")
m_scen1_a <- compute_erfa70(Q_scen1_a, date, "Scen1 (moderate) – Site A")
m_scen1_b <- compute_erfa70(Q_scen1_b, date, "Scen1 (moderate) – Site B")
m_scen2_a <- compute_erfa70(Q_scen2_a, date, "Scen2 (severe) – Site A")
m_scen2_b <- compute_erfa70(Q_scen2_b, date, "Scen2 (severe) – Site B")
m_scen3_a <- compute_erfa70(Q_scen3_a, date, "Scen3 (climate) – Site A")
m_scen3_b <- compute_erfa70(Q_scen3_b, date, "Scen3 (climate) – Site B")
cat("\n")


# ==============================================================================
# SECTION 3: Basic single-site ERFA — uniform sensitivity threshold
# ==============================================================================
# The simplest call: one baseline table vs one scenario table with a single
# sensitivity threshold applied uniformly to all ERFA groups.

erfa_basic <- evaluate_erfa_class(
  baseline_tbl   = m_base_a,
  scenario_tbl   = m_scen1_a,
  sensi_threshold = 30
)

cat("--- Basic ERFA: Site A, Scenario 1 (30% threshold) ---\n")
print(erfa_basic)

# The summary() method extracts the tibble directly
cat("Summary tibble:\n")
summary(erfa_basic) %>%
  select(group, n_total, n_outside, perc_outside, risk_class, risk_label) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 4: Inspecting metric-level detail
# ==============================================================================
# The detail table shows per-metric comparisons: baseline value, scenario value,
# percent change, and whether each metric exceeds its group threshold.

cat("--- Metrics exceeding threshold (sorted by |change|) ---\n")
erfa_basic$detail %>%
  filter(outside_range) %>%
  arrange(desc(abs(pct_change))) %>%
  select(metric_key, group, stat, value_base, value_scen, pct_change) %>%
  head(15) %>%
  print()
cat("\n")

# Which groups are driving the risk?
cat("--- Exceedances by group and stat ---\n")
erfa_basic$detail %>%
  filter(outside_range) %>%
  count(group, stat, name = "n_exceeded") %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 5: Per-group sensitivity thresholds
# ==============================================================================
# Different ERFA groups can have different tolerance levels. Pass a named vector
# with keys HF, MF, LF, RFC, IF.

erfa_custom_thr <- evaluate_erfa_class(
  baseline_tbl   = m_base_a,
  scenario_tbl   = m_scen2_a,
  sensi_threshold = c(HF = 40, MF = 25, LF = 20, RFC = 50, IF = 35)
)

cat("--- Per-group thresholds: Site A, Scenario 2 (severe) ---\n")
print(erfa_custom_thr)
cat("\n")


# ==============================================================================
# SECTION 6: Sensitivity analysis — varying threshold levels
# ==============================================================================
# Sweep a range of uniform thresholds to see how risk class changes.

cat("--- Threshold sensitivity sweep (Site A, Scenario 2) ---\n")
thresholds <- c(10, 20, 30, 40, 50, 75)

sweep_results <- lapply(thresholds, function(thr) {
  res <- evaluate_erfa_class(
    baseline_tbl   = m_base_a,
    scenario_tbl   = m_scen2_a,
    sensi_threshold = thr
  )
  summary(res) %>%
    filter(group == "Overall") %>%
    mutate(threshold = thr)
})

sweep_tbl <- bind_rows(sweep_results) %>%
  select(threshold, n_outside, n_total, perc_outside, risk_class, risk_label)

print(sweep_tbl)
cat("\n")


# ==============================================================================
# SECTION 7: Multi-site ERFA using group_cols
# ==============================================================================
# When multiple sites are computed together, add a "site" column to both tables
# and pass group_cols = "site" to get per-site ERFA results in one call.

baseline_multi <- bind_rows(
  m_base_a %>% mutate(site = "site_A"),
  m_base_b %>% mutate(site = "site_B")
)

scenario_multi <- bind_rows(
  m_scen2_a %>% mutate(site = "site_A"),
  m_scen2_b %>% mutate(site = "site_B")
)

erfa_multi <- evaluate_erfa_class(
  baseline_tbl   = baseline_multi,
  scenario_tbl   = scenario_multi,
  sensi_threshold = 30,
  group_cols      = "site"
)

cat("--- Multi-site ERFA (group_cols = 'site') ---\n")
print(erfa_multi)

# Extract per-site overall risk
summary(erfa_multi) %>%
  filter(group == "Overall") %>%
  select(site, n_outside, risk_class, risk_label) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 8: Multi-scenario ERFA using scenario_cols
# ==============================================================================
# Compare one baseline against multiple scenarios simultaneously by adding a
# scenario identifier column and using scenario_cols.

# Baseline is shared (no scenario column needed)
baseline_single <- m_base_a

# Stack scenarios with a label column
scenarios_stacked <- bind_rows(
  m_scen1_a %>% mutate(scenario = "moderate"),
  m_scen2_a %>% mutate(scenario = "severe"),
  m_scen3_a %>% mutate(scenario = "climate")
)

erfa_multi_scen <- evaluate_erfa_class(
  baseline_tbl   = baseline_single,
  scenario_tbl   = scenarios_stacked,
  sensi_threshold = 30,
  scenario_cols   = "scenario"
)

cat("--- Multi-scenario ERFA (scenario_cols = 'scenario') ---\n")
print(erfa_multi_scen)

# Side-by-side risk comparison
summary(erfa_multi_scen) %>%
  filter(group == "Overall") %>%
  select(scenario, n_outside, perc_outside, risk_class, risk_label) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 9: Combined group_cols × scenario_cols (site × scenario matrix)
# ==============================================================================
# The most powerful pattern: evaluate every site × scenario combination in one
# call. Baseline is broadcast across all scenarios per site.

baseline_all <- bind_rows(
  m_base_a %>% mutate(site = "site_A"),
  m_base_b %>% mutate(site = "site_B")
)

scenarios_all <- bind_rows(
  m_scen1_a %>% mutate(site = "site_A", scenario = "moderate"),
  m_scen2_a %>% mutate(site = "site_A", scenario = "severe"),
  m_scen3_a %>% mutate(site = "site_A", scenario = "climate"),
  m_scen1_b %>% mutate(site = "site_B", scenario = "moderate"),
  m_scen2_b %>% mutate(site = "site_B", scenario = "severe"),
  m_scen3_b %>% mutate(site = "site_B", scenario = "climate")
)

erfa_matrix <- evaluate_erfa_class(
  baseline_tbl   = baseline_all,
  scenario_tbl   = scenarios_all,
  sensi_threshold = 30,
  group_cols      = "site",
  scenario_cols   = "scenario"
)

cat("--- Full site × scenario ERFA matrix ---\n")
print(erfa_matrix)

# Compact risk table
cat("\n--- Risk matrix (Overall class per site × scenario) ---\n")
summary(erfa_matrix) %>%
  filter(group == "Overall") %>%
  select(site, scenario, n_outside, risk_class, risk_label) %>%
  pivot_wider(
    names_from  = scenario,
    values_from = c(n_outside, risk_class),
    names_glue  = "{scenario}_{.value}"
  ) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 10: Examining group-level patterns across scenarios
# ==============================================================================
# For management decisions, it's useful to see which ERFA groups are most
# affected and how this varies by scenario.

cat("--- Group-level risk by scenario (Site A only) ---\n")
summary(erfa_matrix) %>%
  filter(site == "site_A", group != "Overall") %>%
  select(scenario, group, n_outside, n_total, risk_label) %>%
  pivot_wider(
    names_from = scenario,
    values_from = c(n_outside, risk_label),
    names_glue = "{scenario}_{.value}"
  ) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 11: Detail-level analysis — identifying critical metrics
# ==============================================================================
# Which specific metrics are most consistently affected across scenarios?

cat("--- Most frequently exceeded metrics across all scenarios (Site A) ---\n")
erfa_matrix$detail %>%
  filter(grepl("site_A", paste(site))) %>%
  filter(outside_range) %>%
  count(metric_key, group, sort = TRUE) %>%
  head(10) %>%
  print()
cat("\n")

# Largest changes for the severe scenario
cat("--- Largest % changes: Site A, severe scenario ---\n")
erfa_matrix$detail %>%
  filter(site == "site_A") %>%
  filter(outside_range) %>%
  arrange(desc(abs(pct_change))) %>%
  select(metric_key, stat, group, pct_change) %>%
  head(10) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 12: Risk class thresholds reference
# ==============================================================================
# The ERFA classification uses fixed count thresholds per group:
#   Group    | Low (≥) | Medium (≥) | High (≥)
#   ---------|---------|------------|--------
#   Overall  |    1    |     23     |   47
#   HF       |    1    |      6     |   11
#   MF       |    1    |      9     |   17
#   LF       |    1    |      6     |   11
#   RFC      |    1    |      3     |    6
#   IF       |    1    |      3     |    5
#
# Risk labels:
#   0 = No change (🟦 Blue)
#   1 = Low (🟩 Green)
#   2 = Medium (🟧 Amber)
#   3 = High (🟥 Red)

cat("--- ERFA risk class reference ---\n")
tibble::tribble(
  ~group,    ~"Low (≥)", ~"Medium (≥)", ~"High (≥)",
  "Overall",          1,            23,          47,
  "HF",              1,             6,          11,
  "MF",              1,             9,          17,
  "LF",              1,             6,          11,
  "RFC",             1,             3,           6,
  "IF",              1,             3,           5
) %>% print()
cat("\n")


# ==============================================================================
# SECTION 13: Programmatic extraction patterns
# ==============================================================================
# Common patterns for using ERFA results in larger workflows.

res <- erfa_matrix

# Pattern 1: Get overall risk class per site-scenario as a lookup table
risk_lookup <- summary(res) %>%
  filter(group == "Overall") %>%
  select(site, scenario, risk_class)

cat("--- Risk lookup table ---\n")
print(risk_lookup)

# Pattern 2: Flag high-risk combinations
high_risk <- risk_lookup %>% filter(risk_class >= 3)
cat("\nHigh-risk combinations:\n")
if (nrow(high_risk) > 0) print(high_risk) else cat("  None\n")

# Pattern 3: Count total exceedances per group across all combos
cat("\nTotal exceedances by ERFA group (summed across site × scenario):\n")
summary(res) %>%
  filter(group != "Overall") %>%
  group_by(group) %>%
  summarise(
    total_exceedances = sum(n_outside),
    mean_perc_outside = mean(perc_outside),
    max_risk_class    = max(risk_class),
    .groups = "drop"
  ) %>%
  arrange(desc(total_exceedances)) %>%
  print()

# Pattern 4: Export detail for external reporting
detail_export <- res$detail %>%
  select(any_of(c("site", "scenario", "metric_id", "metric_key", "group", "stat",
                  "value_base", "value_scen", "pct_change", "outside_range")))

cat("\nDetail export dimensions:", nrow(detail_export), "×", ncol(detail_export), "\n")

cat("\nERFA pipeline complete.\n")
