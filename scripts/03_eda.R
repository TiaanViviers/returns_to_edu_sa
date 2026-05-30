suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(stringr))


qlfs_path <- "data/processed/qlfs_2023/qlfs_2023_clean.csv"
lmdsa_path <- "data/processed/lmdsa_2023/lmdsa_2023_clean.csv"
tables_dir <- "eda/tables"
figures_dir <- "eda/figures"

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(qlfs_path)) {
  stop(paste("Missing QLFS cleaned file:", qlfs_path), call. = FALSE)
}

if (!file.exists(lmdsa_path)) {
  stop(paste("Missing LMDSA cleaned file:", lmdsa_path), call. = FALSE)
}

# ----------------------------
# Helpers
# ----------------------------
write_table <- function(df, filename) {
  path <- file.path(tables_dir, filename)
  write_csv(df, path)
  cat("Saved table:", path, "\n")
}

save_plot <- function(plot, filename, width = 10, height = 6) {
  path <- file.path(figures_dir, filename)
  ggsave(path, plot, width = width, height = height, dpi = 300)
  cat("Saved figure:", path, "\n")
}

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_sum <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  sum(x[ok] * w[ok])
}

w_ratio <- function(num, den, w) {
  ok <- !is.na(num) & !is.na(den) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)

  numerator <- sum(num[ok] * w[ok])
  denominator <- sum(den[ok] * w[ok])

  if (denominator == 0) return(NA_real_)
  numerator / denominator
}

w_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  ok <- !is.na(x) & !is.na(w) & w > 0

  if (sum(ok) == 0) {
    return(rep(NA_real_, length(probs)))
  }

  x <- x[ok]
  w <- w[ok]

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  cw <- cumsum(w) / sum(w)

  sapply(probs, function(p) {
    x[which(cw >= p)[1]]
  })
}

clean_label <- function(x) {
  str_replace_all(x, "_", " ") |>
    str_to_title()
}

educ_levels <- c(
  "no_schooling",
  "less_than_primary",
  "primary",
  "incomplete_secondary",
  "matric",
  "certificate_diploma",
  "degree_postgrad"
)

educ_labels <- c(
  "No schooling",
  "Less than primary",
  "Primary",
  "Incomplete secondary",
  "Matric",
  "Certificate/diploma",
  "Degree/postgrad"
)

# ----------------------------
# Load data
# ----------------------------
qlfs <- read_csv(qlfs_path, show_col_types = FALSE)
lmdsa <- read_csv(lmdsa_path, show_col_types = FALSE)

# ----------------------------
# Standardise factor ordering / labels for EDA
# ----------------------------
qlfs <- qlfs %>%
  mutate(
    educ_group = factor(educ_group, levels = educ_levels, labels = educ_labels),
    population_group = factor(clean_label(population_group)),
    province = factor(clean_label(province)),
    geo_type = factor(clean_label(geo_type)),
    gender = factor(clean_label(gender)),
    employment_status = factor(clean_label(employment_status))
  )

lmdsa <- lmdsa %>%
  mutate(
    educ_group = factor(educ_group, levels = educ_levels, labels = educ_labels),
    population_group = factor(clean_label(population_group)),
    province = factor(clean_label(province)),
    geo_type = factor(clean_label(geo_type)),
    gender = factor(clean_label(gender)),
    employment_status = factor(clean_label(employment_status))
  )

# Main samples
qlfs_emp <- qlfs %>%
  filter(main_employment_sample_flag == 1)

qlfs_emp_adult <- qlfs_emp %>%
  filter(age >= 25, age <= 64)

lmdsa_wage <- lmdsa %>%
  filter(wage_sample_trimmed_flag == 1)

# Create NEET flag safely. In the public QLFS-derived data this is usually coded as 1 = NEET.
qlfs_emp <- qlfs_emp %>%
  mutate(
    neet_flag = if_else(!is.na(neet) & neet == 1, 1L, 0L)
  )

# ============================================================
# A. Sample overview
# ============================================================

sample_overview <- tibble(
  dataset = c("QLFS 2023 cleaned", "QLFS employment EDA sample", "LMDSA 2023 cleaned", "LMDSA wage sample trimmed"),
  rows = c(nrow(qlfs), nrow(qlfs_emp), nrow(lmdsa), nrow(lmdsa_wage)),
  weighted_total = c(
    sum(qlfs$weight, na.rm = TRUE),
    sum(qlfs_emp$weight, na.rm = TRUE),
    sum(lmdsa$weight, na.rm = TRUE),
    sum(lmdsa_wage$weight, na.rm = TRUE)
  )
)

write_table(sample_overview, "00_sample_overview.csv")

# ============================================================
# B. Education distribution
# ============================================================

education_distribution_qlfs <- qlfs_emp %>%
  group_by(educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share = weighted_n / sum(weighted_n)
  )

write_table(education_distribution_qlfs, "01_qlfs_education_distribution.csv")

p_education_distribution <- ggplot(education_distribution_qlfs, aes(x = educ_group, y = share)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Education distribution among working-age individuals",
    x = "Highest completed education group",
    y = "Weighted share"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_education_distribution, "01_qlfs_education_distribution.png")

education_by_population <- qlfs_emp %>%
  group_by(population_group, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    share_within_population = weighted_n / sum(weighted_n)
  ) %>%
  ungroup()

write_table(education_by_population, "02_qlfs_education_by_population_group.csv")

p_education_by_population <- ggplot(
  education_by_population,
  aes(x = population_group, y = share_within_population, fill = educ_group)
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Education composition by population group",
    x = "Population group",
    y = "Weighted share",
    fill = "Education"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_education_by_population, "02_qlfs_education_by_population_group.png")

education_by_geo <- qlfs_emp %>%
  group_by(geo_type, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    share_within_geo = weighted_n / sum(weighted_n)
  ) %>%
  ungroup()

write_table(education_by_geo, "03_qlfs_education_by_geo_type.csv")

p_education_by_geo <- ggplot(
  education_by_geo,
  aes(x = geo_type, y = share_within_geo, fill = educ_group)
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Education composition by geography type",
    x = "Geography type",
    y = "Weighted share",
    fill = "Education"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_education_by_geo, "03_qlfs_education_by_geo_type.png")

education_high_level_shares <- qlfs_emp %>%
  group_by(population_group, geo_type) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    matric_or_more_share = w_mean(as.integer(educ_group %in% c("Matric", "Certificate/diploma", "Degree/postgrad")), weight),
    tertiary_share = w_mean(as.integer(educ_group %in% c("Certificate/diploma", "Degree/postgrad")), weight),
    degree_share = w_mean(as.integer(educ_group == "Degree/postgrad"), weight),
    .groups = "drop"
  )

write_table(education_high_level_shares, "04_qlfs_high_level_education_shares_by_population_geo.csv")

# ============================================================
# C. Employment outcomes by education - QLFS
# ============================================================

qlfs_employment_by_education <- qlfs_emp %>%
  group_by(educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),

    employment_rate = w_mean(employed, weight),
    unemployed_share_working_age = w_mean(unemployed, weight),
    discouraged_share_working_age = w_mean(discouraged, weight),
    not_economically_active_share = w_mean(not_economically_active, weight),
    neet_rate = w_mean(neet_flag, weight),

    official_unemployment_rate = w_ratio(unemployed, labour_force_flag, weight),
    expanded_unemployment_rate = w_ratio(unemployed + discouraged, expanded_labour_force_flag, weight),

    .groups = "drop"
  )

write_table(qlfs_employment_by_education, "05_qlfs_employment_by_education.csv")

p_employment_rate <- ggplot(qlfs_employment_by_education, aes(x = educ_group, y = employment_rate)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Employment rate by education level",
    subtitle = "Working-age QLFS 2023 sample",
    x = "Highest completed education group",
    y = "Weighted employment rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_employment_rate, "05_qlfs_employment_rate_by_education.png")

p_unemployment_rate <- ggplot(qlfs_employment_by_education, aes(x = educ_group, y = official_unemployment_rate)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Official unemployment rate by education level",
    subtitle = "Unemployed as share of labour force",
    x = "Highest completed education group",
    y = "Weighted unemployment rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_unemployment_rate, "06_qlfs_official_unemployment_rate_by_education.png")

labour_status_distribution <- qlfs_emp %>%
  group_by(educ_group, employment_status) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    share = weighted_n / sum(weighted_n)
  ) %>%
  ungroup()

write_table(labour_status_distribution, "06_qlfs_labour_status_distribution_by_education.csv")

p_labour_status_distribution <- ggplot(
  labour_status_distribution,
  aes(x = educ_group, y = share, fill = employment_status)
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Labour-market status by education level",
    x = "Highest completed education group",
    y = "Weighted share",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_labour_status_distribution, "07_qlfs_labour_status_by_education.png")

# ============================================================
# C2. Employment outcomes by education - QLFS adults 25-64
# ============================================================

qlfs_emp_adult <- qlfs_emp_adult %>%
  mutate(
    neet_flag = if_else(!is.na(neet) & neet == 1, 1L, 0L)
  )

qlfs_employment_by_education_adult <- qlfs_emp_adult %>%
  group_by(educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),

    employment_rate = w_mean(employed, weight),
    unemployed_share_working_age = w_mean(unemployed, weight),
    discouraged_share_working_age = w_mean(discouraged, weight),
    not_economically_active_share = w_mean(not_economically_active, weight),
    neet_rate = w_mean(neet_flag, weight),

    official_unemployment_rate = w_ratio(unemployed, labour_force_flag, weight),
    expanded_unemployment_rate = w_ratio(unemployed + discouraged, expanded_labour_force_flag, weight),

    .groups = "drop"
  )

write_table(qlfs_employment_by_education_adult, "05b_qlfs_employment_by_education_age25_64.csv")

p_employment_rate_adult <- ggplot(
  qlfs_employment_by_education_adult,
  aes(x = educ_group, y = employment_rate)
) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Employment rate by education level",
    subtitle = "QLFS 2023, adults aged 25-64",
    x = "Highest completed education group",
    y = "Weighted employment rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_employment_rate_adult, "05b_qlfs_employment_rate_by_education_age25_64.png")

labour_status_distribution_adult <- qlfs_emp_adult %>%
  group_by(educ_group, employment_status) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    share = weighted_n / sum(weighted_n)
  ) %>%
  ungroup()

write_table(
  labour_status_distribution_adult,
  "06b_qlfs_labour_status_distribution_by_education_age25_64.csv"
)

p_labour_status_distribution_adult <- ggplot(
  labour_status_distribution_adult,
  aes(x = educ_group, y = share, fill = employment_status)
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Labour-market status by education level",
    subtitle = "QLFS 2023, adults aged 25-64",
    x = "Highest completed education group",
    y = "Weighted share",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(
  p_labour_status_distribution_adult,
  "07b_qlfs_labour_status_by_education_age25_64.png"
)

# ============================================================
# D. Employment heterogeneity by population group, gender, geography
# ============================================================

qlfs_employment_by_education_population <- qlfs_emp %>%
  group_by(population_group, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    employment_rate = w_mean(employed, weight),
    official_unemployment_rate = w_ratio(unemployed, labour_force_flag, weight),
    expanded_unemployment_rate = w_ratio(unemployed + discouraged, expanded_labour_force_flag, weight),
    neet_rate = w_mean(neet_flag, weight),
    .groups = "drop"
  )

write_table(qlfs_employment_by_education_population, "07_qlfs_employment_by_education_population.csv")

p_employment_by_population <- qlfs_employment_by_education_population %>%
  filter(n >= 100) %>%
  ggplot(aes(x = educ_group, y = employment_rate, group = population_group, linetype = population_group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Employment rate by education and population group",
    x = "Education group",
    y = "Weighted employment rate",
    linetype = "Population group"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_employment_by_population, "08_qlfs_employment_by_education_population.png")

qlfs_employment_by_education_gender <- qlfs_emp %>%
  group_by(gender, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    employment_rate = w_mean(employed, weight),
    official_unemployment_rate = w_ratio(unemployed, labour_force_flag, weight),
    expanded_unemployment_rate = w_ratio(unemployed + discouraged, expanded_labour_force_flag, weight),
    neet_rate = w_mean(neet_flag, weight),
    .groups = "drop"
  )

write_table(qlfs_employment_by_education_gender, "08_qlfs_employment_by_education_gender.csv")

p_employment_by_gender <- ggplot(
  qlfs_employment_by_education_gender,
  aes(x = educ_group, y = employment_rate, group = gender, linetype = gender)
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Employment rate by education and gender",
    x = "Education group",
    y = "Weighted employment rate",
    linetype = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_employment_by_gender, "09_qlfs_employment_by_education_gender.png")

qlfs_employment_by_education_geo <- qlfs_emp %>%
  group_by(geo_type, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    employment_rate = w_mean(employed, weight),
    official_unemployment_rate = w_ratio(unemployed, labour_force_flag, weight),
    expanded_unemployment_rate = w_ratio(unemployed + discouraged, expanded_labour_force_flag, weight),
    neet_rate = w_mean(neet_flag, weight),
    .groups = "drop"
  )

write_table(qlfs_employment_by_education_geo, "09_qlfs_employment_by_education_geo.csv")

p_employment_by_geo <- ggplot(
  qlfs_employment_by_education_geo,
  aes(x = educ_group, y = employment_rate, group = geo_type, linetype = geo_type)
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Employment rate by education and geography type",
    x = "Education group",
    y = "Weighted employment rate",
    linetype = "Geography type"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_employment_by_geo, "10_qlfs_employment_by_education_geo.png")

# ============================================================
# E. Formal / informal employment by education - QLFS
# ============================================================

qlfs_sector_by_education <- qlfs_emp %>%
  filter(employed == 1) %>%
  group_by(educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    formal_sector_share = w_mean(as.integer(sector2 == 1), weight),
    informal_sector_share = w_mean(as.integer(sector2 == 2), weight),
    private_household_share = w_mean(as.integer(sector2 == 4), weight),
    formal_employment_share = w_mean(as.integer(informal_employment == 1), weight),
    informal_employment_share = w_mean(as.integer(informal_employment == 2), weight),
    mean_hours_worked = w_mean(hours_worked, weight),
    .groups = "drop"
  )

write_table(qlfs_sector_by_education, "10_qlfs_sector_formality_by_education.csv")

p_formal_sector <- ggplot(qlfs_sector_by_education, aes(x = educ_group, y = formal_sector_share)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Formal-sector share among employed workers by education",
    x = "Education group",
    y = "Weighted formal-sector share"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_formal_sector, "11_qlfs_formal_sector_share_by_education.png")

# ============================================================
# F. Wage outcomes by education - LMDSA
# ============================================================

lmdsa_wage_by_education <- lmdsa_wage %>%
  group_by(educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),

    mean_wage = w_mean(employee_monthly_wage, weight),
    mean_log_wage = w_mean(log_employee_wage, weight),

    p25_wage = w_quantile(employee_monthly_wage, weight, 0.25),
    median_wage = w_quantile(employee_monthly_wage, weight, 0.50),
    p75_wage = w_quantile(employee_monthly_wage, weight, 0.75),

    .groups = "drop"
  ) %>%
  mutate(
    median_wage_ratio_to_incomplete_secondary =
      median_wage / median_wage[educ_group == "Incomplete secondary"],
    mean_wage_ratio_to_incomplete_secondary =
      mean_wage / mean_wage[educ_group == "Incomplete secondary"]
  )

write_table(lmdsa_wage_by_education, "11_lmdsa_wage_by_education.csv")

p_median_wage <- ggplot(lmdsa_wage_by_education, aes(x = educ_group, y = median_wage)) +
  geom_col() +
  scale_y_continuous(labels = label_number(prefix = "R", big.mark = ",")) +
  labs(
    title = "Median monthly employee wage by education level",
    subtitle = "Trimmed LMDSA wage sample, employees aged 25–64",
    x = "Education group",
    y = "Weighted median monthly wage"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_median_wage, "12_lmdsa_median_wage_by_education.png")

p_mean_wage <- ggplot(lmdsa_wage_by_education, aes(x = educ_group, y = mean_wage)) +
  geom_col() +
  scale_y_continuous(labels = label_number(prefix = "R", big.mark = ",")) +
  labs(
    title = "Mean monthly employee wage by education level",
    subtitle = "Trimmed LMDSA wage sample, employees aged 25–64",
    x = "Education group",
    y = "Weighted mean monthly wage"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_mean_wage, "13_lmdsa_mean_wage_by_education.png")

p_log_wage_boxplot <- ggplot(lmdsa_wage, aes(x = educ_group, y = log_employee_wage)) +
  geom_boxplot(outlier.alpha = 0.15) +
  labs(
    title = "Log monthly wage distribution by education level",
    subtitle = "Trimmed LMDSA wage sample, employees aged 25–64",
    x = "Education group",
    y = "Log monthly wage"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_log_wage_boxplot, "14_lmdsa_log_wage_boxplot_by_education.png")

# ============================================================
# G. Wage heterogeneity by population group, gender, geography, province
# ============================================================

lmdsa_wage_by_education_population <- lmdsa_wage %>%
  group_by(population_group, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_wage = w_mean(employee_monthly_wage, weight),
    mean_log_wage = w_mean(log_employee_wage, weight),
    p25_wage = w_quantile(employee_monthly_wage, weight, 0.25),
    median_wage = w_quantile(employee_monthly_wage, weight, 0.50),
    p75_wage = w_quantile(employee_monthly_wage, weight, 0.75),
    .groups = "drop"
  )

write_table(lmdsa_wage_by_education_population, "12_lmdsa_wage_by_education_population.csv")

p_wage_by_population <- lmdsa_wage_by_education_population %>%
  filter(n >= 100) %>%
  ggplot(aes(x = educ_group, y = median_wage, group = population_group, linetype = population_group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = label_number(prefix = "R", big.mark = ",")) +
  labs(
    title = "Median wage by education and population group",
    subtitle = "Cells with at least 100 unweighted observations",
    x = "Education group",
    y = "Weighted median monthly wage",
    linetype = "Population group"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_wage_by_population, "15_lmdsa_median_wage_by_education_population.png")

lmdsa_wage_by_education_gender <- lmdsa_wage %>%
  group_by(gender, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_wage = w_mean(employee_monthly_wage, weight),
    mean_log_wage = w_mean(log_employee_wage, weight),
    p25_wage = w_quantile(employee_monthly_wage, weight, 0.25),
    median_wage = w_quantile(employee_monthly_wage, weight, 0.50),
    p75_wage = w_quantile(employee_monthly_wage, weight, 0.75),
    .groups = "drop"
  )

write_table(lmdsa_wage_by_education_gender, "13_lmdsa_wage_by_education_gender.csv")

p_wage_by_gender <- ggplot(
  lmdsa_wage_by_education_gender,
  aes(x = educ_group, y = median_wage, group = gender, linetype = gender)
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = label_number(prefix = "R", big.mark = ",")) +
  labs(
    title = "Median wage by education and gender",
    x = "Education group",
    y = "Weighted median monthly wage",
    linetype = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_wage_by_gender, "16_lmdsa_median_wage_by_education_gender.png")

lmdsa_wage_by_education_geo <- lmdsa_wage %>%
  group_by(geo_type, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_wage = w_mean(employee_monthly_wage, weight),
    mean_log_wage = w_mean(log_employee_wage, weight),
    p25_wage = w_quantile(employee_monthly_wage, weight, 0.25),
    median_wage = w_quantile(employee_monthly_wage, weight, 0.50),
    p75_wage = w_quantile(employee_monthly_wage, weight, 0.75),
    .groups = "drop"
  )

write_table(lmdsa_wage_by_education_geo, "14_lmdsa_wage_by_education_geo.csv")

p_wage_by_geo <- lmdsa_wage_by_education_geo %>%
  filter(n >= 100) %>%
  ggplot(aes(x = educ_group, y = median_wage, group = geo_type, linetype = geo_type)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = label_number(prefix = "R", big.mark = ",")) +
  labs(
    title = "Median wage by education and geography type",
    subtitle = "Cells with at least 100 unweighted observations",
    x = "Education group",
    y = "Weighted median monthly wage",
    linetype = "Geography type"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_wage_by_geo, "17_lmdsa_median_wage_by_education_geo.png")

lmdsa_wage_by_education_province <- lmdsa_wage %>%
  group_by(province, educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_wage = w_mean(employee_monthly_wage, weight),
    mean_log_wage = w_mean(log_employee_wage, weight),
    median_wage = w_quantile(employee_monthly_wage, weight, 0.50),
    .groups = "drop"
  )

write_table(lmdsa_wage_by_education_province, "15_lmdsa_wage_by_education_province.csv")

# Province plot only for matric and degree/postgrad to avoid clutter
p_wage_by_province_selected <- lmdsa_wage_by_education_province %>%
  filter(educ_group %in% c("Matric", "Degree/postgrad")) %>%
  ggplot(aes(x = province, y = median_wage, fill = educ_group)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_number(prefix = "R", big.mark = ",")) +
  labs(
    title = "Median wage by province for matric and degree/postgrad workers",
    x = "Province",
    y = "Weighted median monthly wage",
    fill = "Education"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_wage_by_province_selected, "18_lmdsa_median_wage_by_province_selected_education.png", width = 11, height = 6)

# ============================================================
# H. Occupation, industry, and sector sorting
# ============================================================

occupation_by_education <- lmdsa_wage %>%
  group_by(educ_group, occupation_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    share_within_education = weighted_n / sum(weighted_n)
  ) %>%
  ungroup()

write_table(occupation_by_education, "16_lmdsa_occupation_distribution_by_education.csv")

industry_by_education <- lmdsa_wage %>%
  group_by(educ_group, industry_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    share_within_education = weighted_n / sum(weighted_n)
  ) %>%
  ungroup()

write_table(industry_by_education, "17_lmdsa_industry_distribution_by_education.csv")

sector_by_education_lmdsa <- lmdsa_wage %>%
  group_by(educ_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    formal_sector_share = w_mean(as.integer(sector2 == 1), weight),
    informal_sector_share = w_mean(as.integer(sector2 == 2), weight),
    private_household_share = w_mean(as.integer(sector2 == 4), weight),
    formal_employment_share = w_mean(as.integer(informal_employment == 1), weight),
    informal_employment_share = w_mean(as.integer(informal_employment == 2), weight),
    mean_hours_worked = w_mean(hours_worked, weight),
    .groups = "drop"
  )

write_table(sector_by_education_lmdsa, "18_lmdsa_sector_formality_by_education.csv")

# ============================================================
# I. Quality/quantity proxy summaries
# ============================================================

quality_proxy_summary <- qlfs_emp %>%
  group_by(geo_type, population_group) %>%
  summarise(
    n = n(),
    weighted_n = sum(weight, na.rm = TRUE),
    employment_rate = w_mean(employed, weight),
    official_unemployment_rate = w_ratio(unemployed, labour_force_flag, weight),
    matric_or_more_share = w_mean(as.integer(educ_group %in% c("Matric", "Certificate/diploma", "Degree/postgrad")), weight),
    tertiary_share = w_mean(as.integer(educ_group %in% c("Certificate/diploma", "Degree/postgrad")), weight),
    degree_share = w_mean(as.integer(educ_group == "Degree/postgrad"), weight),
    .groups = "drop"
  )

write_table(quality_proxy_summary, "19_qlfs_quality_proxy_summary_geo_population.csv")

p_tertiary_by_geo_population <- quality_proxy_summary %>%
  ggplot(aes(x = geo_type, y = tertiary_share, group = population_group, linetype = population_group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Tertiary education share by geography type and population group",
    x = "Geography type",
    y = "Weighted tertiary share",
    linetype = "Population group"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_tertiary_by_geo_population, "19_qlfs_tertiary_share_by_geo_population.png")

quality_proxy_summary_filtered <- quality_proxy_summary %>%
  filter(n >= 500) %>%
  group_by(population_group) %>%
  mutate(n_geo_types = n_distinct(geo_type)) %>%
  ungroup()

write_table(
  select(quality_proxy_summary_filtered, -n_geo_types),
  "19b_qlfs_quality_proxy_summary_geo_population_n500.csv"
)

p_tertiary_by_geo_population_filtered <- ggplot(
  quality_proxy_summary_filtered,
  aes(x = geo_type, y = tertiary_share, group = population_group, color = population_group, linetype = population_group)
) +
  geom_line(data = filter(quality_proxy_summary_filtered, n_geo_types >= 2)) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Tertiary education share by geography type and population group",
    subtitle = "Only cells with at least 500 unweighted observations",
    x = "Geography type",
    y = "Weighted tertiary share",
    color = "Population group",
    linetype = "Population group"
  ) +
  theme_minimal(base_size = 12)

save_plot(
  p_tertiary_by_geo_population_filtered,
  "19b_qlfs_tertiary_share_by_geo_population_n500.png"
)

# ============================================================
# K. Premium summary tables for report interpretation
# ============================================================

education_premium_summary <- qlfs_employment_by_education_adult %>%
  select(
    educ_group,
    qlfs_n = n,
    employment_rate_25_64 = employment_rate,
    official_unemployment_rate_25_64 = official_unemployment_rate,
    expanded_unemployment_rate_25_64 = expanded_unemployment_rate
  ) %>%
  left_join(
    lmdsa_wage_by_education %>%
      select(
        educ_group,
        lmdsa_wage_n = n,
        median_wage,
        mean_wage,
        p25_wage,
        p75_wage
      ),
    by = "educ_group"
  ) %>%
  mutate(
    employment_ratio_to_incomplete_secondary =
      employment_rate_25_64 /
        employment_rate_25_64[educ_group == "Incomplete secondary"],

    official_unemployment_ratio_to_incomplete_secondary =
      official_unemployment_rate_25_64 /
        official_unemployment_rate_25_64[educ_group == "Incomplete secondary"],

    median_wage_ratio_to_incomplete_secondary =
      median_wage /
        median_wage[educ_group == "Incomplete secondary"],

    mean_wage_ratio_to_incomplete_secondary =
      mean_wage /
        mean_wage[educ_group == "Incomplete secondary"],

    employment_rate_pct = employment_rate_25_64 * 100,
    official_unemployment_rate_pct = official_unemployment_rate_25_64 * 100,
    expanded_unemployment_rate_pct = expanded_unemployment_rate_25_64 * 100
  )

write_table(
  education_premium_summary,
  "20_education_premium_summary_employment_and_wages.csv"
)

education_premium_summary_report <- education_premium_summary %>%
  transmute(
    education_group = educ_group,
    qlfs_n,
    lmdsa_wage_n,
    employment_rate_pct = round(employment_rate_pct, 1),
    official_unemployment_rate_pct = round(official_unemployment_rate_pct, 1),
    expanded_unemployment_rate_pct = round(expanded_unemployment_rate_pct, 1),
    employment_ratio_to_incomplete_secondary =
      round(employment_ratio_to_incomplete_secondary, 2),
    median_wage = round(median_wage, 0),
    mean_wage = round(mean_wage, 0),
    p25_wage = round(p25_wage, 0),
    p75_wage = round(p75_wage, 0),
    median_wage_ratio_to_incomplete_secondary =
      round(median_wage_ratio_to_incomplete_secondary, 2),
    mean_wage_ratio_to_incomplete_secondary =
      round(mean_wage_ratio_to_incomplete_secondary, 2)
  )

write_table(
  education_premium_summary_report,
  "20b_education_premium_summary_report_ready.csv"
)

population_wage_premium_summary <- lmdsa_wage_by_education_population %>%
  group_by(population_group) %>%
  mutate(
    median_wage_incomplete_secondary =
      median_wage[educ_group == "Incomplete secondary"][1],
    mean_wage_incomplete_secondary =
      mean_wage[educ_group == "Incomplete secondary"][1],

    median_wage_ratio_to_incomplete_secondary =
      median_wage / median_wage_incomplete_secondary,
    mean_wage_ratio_to_incomplete_secondary =
      mean_wage / mean_wage_incomplete_secondary
  ) %>%
  ungroup()

write_table(
  population_wage_premium_summary,
  "21_population_wage_premium_summary.csv"
)

population_wage_premium_summary_report <- population_wage_premium_summary %>%
  transmute(
    population_group,
    education_group = educ_group,
    n,
    median_wage = round(median_wage, 0),
    mean_wage = round(mean_wage, 0),
    p25_wage = round(p25_wage, 0),
    p75_wage = round(p75_wage, 0),
    median_wage_ratio_to_incomplete_secondary =
      round(median_wage_ratio_to_incomplete_secondary, 2),
    mean_wage_ratio_to_incomplete_secondary =
      round(mean_wage_ratio_to_incomplete_secondary, 2)
  )

write_table(
  population_wage_premium_summary_report,
  "21b_population_wage_premium_summary_report_ready.csv"
)

credential_jump_summary <- education_premium_summary %>%
  summarise(
    employment_incomplete_secondary =
      employment_rate_25_64[educ_group == "Incomplete secondary"],
    employment_matric =
      employment_rate_25_64[educ_group == "Matric"],
    employment_certificate_diploma =
      employment_rate_25_64[educ_group == "Certificate/diploma"],
    employment_degree_postgrad =
      employment_rate_25_64[educ_group == "Degree/postgrad"],

    median_wage_incomplete_secondary =
      median_wage[educ_group == "Incomplete secondary"],
    median_wage_matric =
      median_wage[educ_group == "Matric"],
    median_wage_certificate_diploma =
      median_wage[educ_group == "Certificate/diploma"],
    median_wage_degree_postgrad =
      median_wage[educ_group == "Degree/postgrad"]
  ) %>%
  mutate(
    employment_jump_matric_vs_incomplete_secondary_pp =
      100 * (employment_matric - employment_incomplete_secondary),

    employment_jump_certificate_vs_matric_pp =
      100 * (employment_certificate_diploma - employment_matric),

    employment_jump_degree_vs_matric_pp =
      100 * (employment_degree_postgrad - employment_matric),

    wage_jump_matric_vs_incomplete_secondary =
      median_wage_matric - median_wage_incomplete_secondary,

    wage_jump_certificate_vs_matric =
      median_wage_certificate_diploma - median_wage_matric,

    wage_jump_degree_vs_matric =
      median_wage_degree_postgrad - median_wage_matric,

    wage_ratio_matric_vs_incomplete_secondary =
      median_wage_matric / median_wage_incomplete_secondary,

    wage_ratio_certificate_vs_matric =
      median_wage_certificate_diploma / median_wage_matric,

    wage_ratio_degree_vs_matric =
      median_wage_degree_postgrad / median_wage_matric
  )

write_table(
  credential_jump_summary,
  "22_credential_jump_summary.csv"
)

credential_jump_summary_report <- credential_jump_summary %>%
  transmute(
    employment_jump_matric_vs_incomplete_secondary_pp =
      round(employment_jump_matric_vs_incomplete_secondary_pp, 1),
    employment_jump_certificate_vs_matric_pp =
      round(employment_jump_certificate_vs_matric_pp, 1),
    employment_jump_degree_vs_matric_pp =
      round(employment_jump_degree_vs_matric_pp, 1),

    wage_jump_matric_vs_incomplete_secondary =
      round(wage_jump_matric_vs_incomplete_secondary, 0),
    wage_jump_certificate_vs_matric =
      round(wage_jump_certificate_vs_matric, 0),
    wage_jump_degree_vs_matric =
      round(wage_jump_degree_vs_matric, 0),

    wage_ratio_matric_vs_incomplete_secondary =
      round(wage_ratio_matric_vs_incomplete_secondary, 2),
    wage_ratio_certificate_vs_matric =
      round(wage_ratio_certificate_vs_matric, 2),
    wage_ratio_degree_vs_matric =
      round(wage_ratio_degree_vs_matric, 2)
  )

write_table(
  credential_jump_summary_report,
  "22b_credential_jump_summary_report_ready.csv"
)

# ============================================================
# J. Console summary
# ============================================================

cat("\n================ EDA COMPLETE ================\n")

cat("\nQLFS employment by education:\n")
print(
  qlfs_employment_by_education %>%
    select(
      educ_group,
      n,
      employment_rate,
      official_unemployment_rate,
      expanded_unemployment_rate,
      neet_rate
    )
)

cat("\nQLFS employment by education, adults 25-64:\n")
print(
  qlfs_employment_by_education_adult %>%
    select(
      educ_group,
      n,
      employment_rate,
      official_unemployment_rate,
      expanded_unemployment_rate,
      neet_rate
    )
)

cat("\nLMDSA wage by education:\n")
print(
  lmdsa_wage_by_education %>%
    select(
      educ_group,
      n,
      median_wage,
      mean_wage,
      median_wage_ratio_to_incomplete_secondary
    )
)

cat("\nEducation premium summary, report-ready:\n")
print(education_premium_summary_report)

cat("\nCredential jump summary, report-ready:\n")
print(credential_jump_summary_report)

cat("\nPopulation wage premium summary, report-ready:\n")
print(
  population_wage_premium_summary_report %>%
    filter(education_group %in% c("Incomplete secondary", "Matric", "Certificate/diploma", "Degree/postgrad"))
)

cat("\nTables saved to:", tables_dir, "\n")
cat("Figures saved to:", figures_dir, "\n")
cat("==============================================\n")
