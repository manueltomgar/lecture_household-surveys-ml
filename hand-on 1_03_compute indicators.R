# Project       : Lecture on using statistical learning models with household survey microdata
# Last update   : 09/06/2026
# Author        : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution   : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Creates a simplified dataset from original HBS microdata
# Compute descriptive statistics, correlations, figures, and policy indicators

# [] Preliminaries ----

# Clear workspace
rm(list = ls(all = TRUE))

# Install and load any required R-package
packages_loaded <- installed.packages()
packages_needed <- c ( "dplyr"       ,
                       "openxlsx"    ,
                       "reshape2"    ,
                       "tidyr"       , 
                       "ggplot2"     , 
                       "readr"       , 
                       "stringr"     ,
                       "scales"      ,
                       "stringr"     ,
                       "here"        )
for ( p in packages_needed) {
  if (!p %in% row.names(packages_loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

# Define main path
path <- here()

# Set working directory
setwd(path)

# Load string vectors with auxiliary information needed
vectors <- read.table('vectors.csv',
                      header = TRUE,
                      sep = ',',
                      stringsAsFactors = FALSE,
                      skipNul = TRUE,
                      blank.lines.skip = TRUE)
for (a in names(vectors)) {
  eval(parse(text = paste0(a, " <- vectors[!is.na(vectors$", a,") & nchar(vectors$", a, ") > 0,]$", a)))
}

# Create output folders
if (!dir.exists(file.path(path, "outputs"))) dir.create(file.path(path, "outputs"))
if (!dir.exists(file.path(path, "outputs", "indicators"))) dir.create(file.path(path, "outputs", "indicators"))
if (!dir.exists(file.path(path, "outputs", "figures"))) dir.create(file.path(path, "outputs", "figures"))

# [] Create the simplified dataset ----
ml_file <- file.path(path, "data", "hbs_energy_ml_2023.csv")
hbs_file <- file.path(path, "data", "hbs_h_2023.csv")

  # Load the household-level HBS file created in script 1_02
  hbs_h <- read_csv(hbs_file, show_col_types = FALSE)

  # Electricity expenditure codes for main and secondary dwellings
  home_electricity_cols <- c(
    "EUR_HE04511", # Electricity, main dwelling
    "EUR_HE04512"  # Electricity, other dwellings
  )

  # Gas expenditure codes: natural gas, city gas and LPG
  home_gas_cols <- c(
    "EUR_HE04521", # Natural/city gas, main dwelling
    "EUR_HE04522", # Natural/city gas, other dwellings
    "EUR_HE04523", # LPG, main dwelling
    "EUR_HE04524"  # LPG, other dwellings
  )

  # Other domestic fuels: liquid fuels, coal and other solid fuels
  home_other_fuel_cols <- c(
    "EUR_HE04531", # Liquid fuels, main dwelling
    "EUR_HE04532", # Liquid fuels, other dwellings
    "EUR_HE04541", # Coal, main dwelling
    "EUR_HE04542", # Coal, other dwellings
    "EUR_HE04548", # Other solid fuels, main dwelling
    "EUR_HE04549"  # Other solid fuels, other dwellings
  )

  # Private transport fuel expenditure codes
  vehicle_fuel_cols <- c(
    "EUR_HE07221", # Diesel
    "EUR_HE07222", # Petrol / gasoline
    "EUR_HE07223"  # Other vehicle fuels, including electricity for EVs
  )

  # Helper function to label dwelling energy source codes
  energy_source_label <- function(has_service, source_code, no_service_label) {
    case_when(
      has_service == 6 ~ no_service_label,
      source_code == 1 ~ "Electricity",
      source_code == 2 ~ "Natural gas",
      source_code == 3 ~ "LPG",
      source_code == 4 ~ "Other liquid fuels",
      source_code == 5 ~ "Solid fuels",
      source_code == 6 ~ "Other",
      source_code == -9 ~ NA_character_,
      TRUE ~ NA_character_
    )
  }

  # Build the ML-ready dataset used in the course
  hbs_energy_ml <- hbs_h %>%
    mutate(
      home_electricity_spend_eur = rowSums(across(all_of(home_electricity_cols)), na.rm = TRUE),
      home_gas_spend_eur         = rowSums(across(all_of(home_gas_cols)), na.rm = TRUE),
      home_other_fuels_spend_eur = rowSums(across(all_of(home_other_fuel_cols)), na.rm = TRUE),

      home_energy_spend_eur =
        home_electricity_spend_eur +
        home_gas_spend_eur +
        home_other_fuels_spend_eur,

      private_vehicle_fuel_spend_eur = rowSums(across(all_of(vehicle_fuel_cols)), na.rm = TRUE),

      vehicle_diesel_spend_eur = EUR_HE07221,
      vehicle_petrol_spend_eur = EUR_HE07222,

      heating_energy_source = energy_source_label(
        has_service = CALEF,
        source_code = FUENCALE,
        no_service_label = "No heating"
      ),

      hot_water_energy_source = energy_source_label(
        has_service = AGUACALI,
        source_code = FUENAGUA,
        no_service_label = "No hot water"
      )
    ) %>%
    transmute(
      # Survey identifiers and survey weight
      survey_year = ANOENC,
      household_id = NUMERO,
      survey_weight = FACTOR,

      # Total consumption and energy expenditure variables
      total_consumption_spend_eur = EUR_HE00,
      home_energy_spend_eur,
      home_electricity_spend_eur,
      home_gas_spend_eur,
      home_other_fuels_spend_eur,
      private_vehicle_fuel_spend_eur,
      vehicle_diesel_spend_eur,
      vehicle_petrol_spend_eur,

      # Household socioeconomic variables
      monthly_net_income_eur = IMPEXAC,
      household_size = NMIEMB,
      children_under18 = NMIEM5,
      employed_members = NUMOCU,

      # Geographic and dwelling characteristics
      region_code = CCAA,
      municipality_size_code = TAMAMU,
      dwelling_area_m2 = if_else(SUPERF == -9, NA_real_, as.numeric(SUPERF)),

      # Energy system characteristics of the dwelling
      heating_energy_source,
      hot_water_energy_source
    )

  # Save the ML-ready dataset for future scripts
  write_csv(hbs_energy_ml, ml_file)

# [] Define weighted statistics functions ----

# Weighted mean, ignoring missing values
weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Weighted standard deviation, ignoring missing values
weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  mu <- weighted_mean(x[ok], w[ok])
  sqrt(sum(w[ok] * (x[ok] - mu)^2) / sum(w[ok]))
}

# Weighted quantile, ignoring missing values
weighted_quantile <- function(x, w, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)

  sapply(probs, function(p) x[which(cum_w >= p)[1]])
}

# Weighted correlation between two variables
weighted_correlation <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  if (sum(ok) < 2) return(NA_real_)

  x <- x[ok]
  y <- y[ok]
  w <- w[ok]

  mx <- weighted_mean(x, w)
  my <- weighted_mean(y, w)

  cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
  var_x  <- sum(w * (x - mx)^2) / sum(w)
  var_y  <- sum(w * (y - my)^2) / sum(w)

  cov_xy / sqrt(var_x * var_y)
}

# [] Compute policy indicators ----

# Scenario parameter: homogeneous gas price increase
# Example: 0.30 means a 30% increase in gas prices.
gas_price_shock <- 0.30

hbs_energy_ml <- hbs_energy_ml %>%
  mutate(
    # Annual household income
    annual_net_income_eur = if_else(monthly_net_income_eur > 0,
                                    monthly_net_income_eur * 12,
                                    NA_real_),

    # Household energy burden: domestic energy expenditure as a share of income
    home_energy_burden = home_energy_spend_eur / annual_net_income_eur,

    # 10% energy poverty indicator
    energy_poor_10pct = if_else(home_energy_burden > 0.10, 1, 0, missing = NA_real_),

    # Gas price shock: no behavioural response and homogeneous prices
    gas_price_shock_extra_cost_eur = home_gas_spend_eur * gas_price_shock,
    home_energy_spend_after_gas_shock_eur = home_energy_spend_eur + gas_price_shock_extra_cost_eur,
    home_energy_burden_after_gas_shock = home_energy_spend_after_gas_shock_eur / annual_net_income_eur,
    additional_burden_from_gas_shock = home_energy_burden_after_gas_shock - home_energy_burden,

    # Policy-relevant affordability gap:
    # how many euros would be needed to bring energy expenditure down to 10% of income?
    affordability_gap_10pct_eur = pmax(home_energy_spend_eur - 0.10 * annual_net_income_eur, 0),

    # Additional exercise indicator:
    # private transport fuel burden and a 5% high-burden rule
    private_transport_fuel_burden = private_vehicle_fuel_spend_eur / annual_net_income_eur,
    high_transport_fuel_burden_5pct = if_else(private_transport_fuel_burden > 0.05,
                                              1,
                                              0,
                                              missing = NA_real_)
  )

# Weighted income quintiles
income_quintile_breaks <- weighted_quantile(
  hbs_energy_ml$annual_net_income_eur,
  hbs_energy_ml$survey_weight,
  probs = c(0.2, 0.4, 0.6, 0.8)
)

hbs_energy_ml <- hbs_energy_ml %>%
  mutate(
    income_quintile = cut(
      annual_net_income_eur,
      breaks = c(-Inf, income_quintile_breaks, Inf),
      labels = c("Q1 lowest", "Q2", "Q3", "Q4", "Q5 highest"),
      include.lowest = TRUE
    )
  )

# Save the dataset with indicators
write_csv(hbs_energy_ml, file.path(path, "outputs", "indicators", "hbs_energy_ml_with_indicators_2023.csv"))

# [] Descriptive statistics for all variables ----

# Numeric variables: weighted descriptive statistics
numeric_vars <- hbs_energy_ml %>%
  select(where(is.numeric)) %>%
  names()

numeric_descriptives <- bind_rows(lapply(numeric_vars, function(v) {
  x <- hbs_energy_ml[[v]]
  w <- hbs_energy_ml$survey_weight
  qs <- weighted_quantile(x, w, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

  data.frame(
    variable = v,
    n = sum(!is.na(x)),
    missing = sum(is.na(x)),
    weighted_mean = weighted_mean(x, w),
    weighted_sd = weighted_sd(x, w),
    weighted_p10 = qs[1],
    weighted_p25 = qs[2],
    weighted_median = qs[3],
    weighted_p75 = qs[4],
    weighted_p90 = qs[5]
  )
}))

write_csv(numeric_descriptives,
          file.path(path, "outputs", "indicators", "descriptive_statistics_numeric.csv"))

# Categorical variables: weighted shares
categorical_vars <- hbs_energy_ml %>%
  select(where(function(x) is.character(x) | is.factor(x))) %>%
  names()

categorical_descriptives <- bind_rows(lapply(categorical_vars, function(v) {
  hbs_energy_ml %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(value = .data[[v]]) %>%
    summarise(
      variable = v,
      n = n(),
      weighted_households = sum(survey_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_share = weighted_households / sum(weighted_households),
      variable = v
    ) %>%
    select(variable, value, n, weighted_households, weighted_share)
}))

write_csv(categorical_descriptives,
          file.path(path, "outputs", "indicators", "descriptive_statistics_categorical.csv"))

# [] Weighted correlations ----

# Select variables where a correlation is substantively meaningful
correlation_vars <- c(
  "total_consumption_spend_eur",
  "home_energy_spend_eur",
  "home_electricity_spend_eur",
  "home_gas_spend_eur",
  "private_vehicle_fuel_spend_eur",
  "monthly_net_income_eur",
  "household_size",
  "children_under18",
  "employed_members",
  "dwelling_area_m2",
  "home_energy_burden",
  "private_transport_fuel_burden"
)

correlation_matrix <- matrix(
  NA_real_,
  nrow = length(correlation_vars),
  ncol = length(correlation_vars),
  dimnames = list(correlation_vars, correlation_vars)
)

for (i in seq_along(correlation_vars)) {
  for (j in seq_along(correlation_vars)) {
    correlation_matrix[i, j] <- weighted_correlation(
      hbs_energy_ml[[correlation_vars[i]]],
      hbs_energy_ml[[correlation_vars[j]]],
      hbs_energy_ml$survey_weight
    )
  }
}

correlation_table <- as.data.frame(correlation_matrix) %>%
  mutate(variable = rownames(correlation_matrix)) %>%
  relocate(variable)

write_csv(correlation_table,
          file.path(path, "outputs", "indicators", "weighted_correlation_matrix.csv"))

# [] Distributional policy tables ----

# Weighted summary table by income quintile
policy_by_income_quintile <- hbs_energy_ml %>%
  filter(!is.na(income_quintile)) %>%
  group_by(income_quintile) %>%
  summarise(
    weighted_households = sum(survey_weight, na.rm = TRUE),
    mean_annual_income_eur = weighted_mean(annual_net_income_eur, survey_weight),
    mean_home_energy_spend_eur = weighted_mean(home_energy_spend_eur, survey_weight),
    mean_home_energy_burden = weighted_mean(home_energy_burden, survey_weight),
    energy_poverty_rate_10pct = weighted_mean(energy_poor_10pct, survey_weight),
    mean_gas_extra_cost_eur = weighted_mean(gas_price_shock_extra_cost_eur, survey_weight),
    mean_additional_burden_from_gas_shock = weighted_mean(additional_burden_from_gas_shock, survey_weight),
    mean_affordability_gap_10pct_eur = weighted_mean(affordability_gap_10pct_eur, survey_weight),
    high_transport_fuel_burden_5pct_rate = weighted_mean(high_transport_fuel_burden_5pct, survey_weight),
    .groups = "drop"
  )

write_csv(policy_by_income_quintile,
          file.path(path, "outputs", "indicators", "policy_indicators_by_income_quintile.csv"))

# Headline weighted indicators
headline_indicators <- data.frame(
  indicator = c(
    "Mean annual home energy expenditure",
    "Mean household energy burden",
    "Energy poverty rate, 10 percent rule",
    "Mean extra annual cost from gas price shock",
    "Mean additional burden from gas price shock",
    "Mean affordability gap, 10 percent threshold",
    "High private transport fuel burden rate, 5 percent rule"
  ),
  value = c(
    weighted_mean(hbs_energy_ml$home_energy_spend_eur, hbs_energy_ml$survey_weight),
    weighted_mean(hbs_energy_ml$home_energy_burden, hbs_energy_ml$survey_weight),
    weighted_mean(hbs_energy_ml$energy_poor_10pct, hbs_energy_ml$survey_weight),
    weighted_mean(hbs_energy_ml$gas_price_shock_extra_cost_eur, hbs_energy_ml$survey_weight),
    weighted_mean(hbs_energy_ml$additional_burden_from_gas_shock, hbs_energy_ml$survey_weight),
    weighted_mean(hbs_energy_ml$affordability_gap_10pct_eur, hbs_energy_ml$survey_weight),
    weighted_mean(hbs_energy_ml$high_transport_fuel_burden_5pct, hbs_energy_ml$survey_weight)
  )
)

write_csv(headline_indicators,
          file.path(path, "outputs", "indicators", "headline_policy_indicators.csv"))

# [] Figures ----

# Figure 1: distribution of household energy burden
p1 <- hbs_energy_ml %>%
  filter(!is.na(home_energy_burden), home_energy_burden <= 0.30) %>%
  ggplot(aes(x = home_energy_burden, weight = survey_weight)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of household energy burden",
    subtitle = "Dashed line: 10% energy poverty threshold",
    x = "Home energy expenditure / annual net income",
    y = "Weighted number of households"
  ) +
  theme_minimal()

ggsave(file.path(path, "outputs", "figures", "figure_01_energy_burden_distribution.png"),
       p1, width = 9, height = 5, dpi = 300)

# Figure 2: energy poverty by income quintile
p2 <- policy_by_income_quintile %>%
  ggplot(aes(x = income_quintile, y = energy_poverty_rate_10pct)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Energy poverty rate by income quintile",
    subtitle = "10% rule, weighted by FACTOR",
    x = "Income quintile",
    y = "Energy poverty rate"
  ) +
  theme_minimal()

ggsave(file.path(path, "outputs", "figures", "figure_02_energy_poverty_by_income_quintile.png"),
       p2, width = 9, height = 5, dpi = 300)

# Figure 3: gas price shock impact by income quintile
p3 <- policy_by_income_quintile %>%
  ggplot(aes(x = income_quintile, y = mean_additional_burden_from_gas_shock)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Distributional impact of a gas price increase",
    subtitle = paste0("Scenario: +", gas_price_shock * 100, "% gas prices, no household response"),
    x = "Income quintile",
    y = "Additional energy burden"
  ) +
  theme_minimal()

ggsave(file.path(path, "outputs", "figures", "figure_03_gas_price_shock_by_income_quintile.png"),
       p3, width = 9, height = 5, dpi = 300)

# Figure 4: correlation heatmap
correlation_long <- as.data.frame(correlation_matrix) %>%
  mutate(var1 = rownames(correlation_matrix)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p4 <- correlation_long %>%
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(
    title = "Weighted correlation matrix",
    subtitle = "Selected ML variables and policy indicators",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(path, "outputs", "figures", "figure_04_weighted_correlation_heatmap.png"),
       p4, width = 9, height = 7, dpi = 300)

# [] Student exercise ----

# Exercise question:
# Create and interpret an indicator of private transport fuel vulnerability.
#
# Suggested definition:
# A household has high private transport fuel burden if private vehicle fuel expenditure
# represents more than 5% of annual net household income.
#
# Discussion questions:
# 1. Which income quintile has the highest rate?
# 2. Is the pattern the same as for domestic energy poverty?
# 3. How would the indicator change if the threshold were 3% instead of 5%?
# 4. Why is FACTOR needed when reporting population-level results?

transport_exercise <- policy_by_income_quintile %>%
  select(
    income_quintile,
    weighted_households,
    high_transport_fuel_burden_5pct_rate
  )

write_csv(transport_exercise,
          file.path(path, "outputs", "indicators", "student_exercise_transport_fuel_burden.csv"))

# [] End ----