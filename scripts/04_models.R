suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(survey))

# ============================================================
# 04_models.R
# Regression models for returns to education in South Africa
# ============================================================

options(survey.lonely.psu = "adjust")

# ----------------------------
# Paths
# ----------------------------
qlfs_path <- "data/processed/qlfs_2023/qlfs_2023_clean.csv"
lmdsa_path <- "data/processed/lmdsa_2023/lmdsa_2023_clean.csv"

tables_dir <- "report/tables"
figures_dir <- "report/figures"
models_dir <- "report/models"

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

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

clean_label <- function(x) {
  str_replace_all(x, "_", " ") |>
    str_to_title()
}

recode_educ_group <- function(x) {
  case_when(
    x == "no_schooling" ~ "No schooling",
    x == "less_than_primary" ~ "Less than primary",
    x == "primary" ~ "Primary",
    x == "incomplete_secondary" ~ "Incomplete secondary",
    x == "matric" ~ "Matric",
    x == "certificate_diploma" ~ "Certificate/diploma",
    x == "degree_postgrad" ~ "Degree/postgrad",
    TRUE ~ NA_character_
  )
}

tidy_svyglm <- function(model, model_name) {
  coef_mat <- summary(model)$coefficients

  tibble(
    model = model_name,
    term = rownames(coef_mat),
    estimate = coef_mat[, "Estimate"],
    std_error = coef_mat[, "Std. Error"],
    statistic = coef_mat[, "t value"],
    p_value = coef_mat[, "Pr(>|t|)"]
  ) %>%
    mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    )
}

extract_education_effects <- function(tidy_df, outcome_type = c("log_wage", "lpm")) {
  outcome_type <- match.arg(outcome_type)

  out <- tidy_df %>%
    filter(str_detect(term, "^educ_group"), !str_detect(term, ":")) %>%
    mutate(
      education_group = str_remove(term, "^educ_group")
    )

  if (outcome_type == "log_wage") {
    out <- out %>%
      mutate(
        effect = 100 * (exp(estimate) - 1),
        effect_low = 100 * (exp(conf_low) - 1),
        effect_high = 100 * (exp(conf_high) - 1),
        effect_label = "Percent wage premium relative to incomplete secondary"
      )
  }

  if (outcome_type == "lpm") {
    out <- out %>%
      mutate(
        effect = 100 * estimate,
        effect_low = 100 * conf_low,
        effect_high = 100 * conf_high,
        effect_label = "Percentage-point employment association relative to incomplete secondary"
      )
  }

  out
}

add_reference_row <- function(df, model_name, outcome_type = c("log_wage", "lpm")) {
  outcome_type <- match.arg(outcome_type)

  ref_label <- if (outcome_type == "log_wage") {
    "Percent wage premium relative to incomplete secondary"
  } else {
    "Percentage-point employment association relative to incomplete secondary"
  }

  bind_rows(
    tibble(
      model = model_name,
      term = "educ_groupIncomplete secondary",
      estimate = 0,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      conf_low = 0,
      conf_high = 0,
      education_group = "Incomplete secondary",
      effect = 0,
      effect_low = 0,
      effect_high = 0,
      effect_label = ref_label
    ),
    df
  )
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ as.character(round(p, 3))
  )
}

education_order <- c(
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
qlfs <- read_csv(
  qlfs_path,
  col_types = cols(
    person_id = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

lmdsa <- read_csv(
  lmdsa_path,
  col_types = cols(
    person_id = col_character(),
    psu = col_character(),
    stratum = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

# ----------------------------
# Prepare model datasets
# ----------------------------

lmdsa_model <- lmdsa %>%
  filter(wage_sample_trimmed_flag == 1) %>%
  mutate(
    educ_group = factor(recode_educ_group(educ_group), levels = education_order),
    educ_group = fct_relevel(educ_group, "Incomplete secondary"),

    gender = factor(clean_label(gender)),
    population_group = factor(clean_label(population_group)),
    province = factor(clean_label(province)),
    geo_type = factor(clean_label(geo_type)),

    occupation_group = factor(occupation_group),
    industry_group = factor(industry_group),
    sector2 = factor(sector2),

    log_employee_wage = as.numeric(log_employee_wage),
    potential_experience = as.numeric(potential_experience),
    potential_experience_sq = as.numeric(potential_experience_sq),
    hours_worked = as.numeric(hours_worked),
    weight = as.numeric(weight)
  ) %>%
  filter(
    !is.na(log_employee_wage),
    !is.na(educ_group),
    !is.na(potential_experience),
    !is.na(potential_experience_sq),
    !is.na(gender),
    !is.na(population_group),
    !is.na(province),
    !is.na(geo_type),
    !is.na(weight),
    weight > 0,
    !is.na(stratum),
    !is.na(psu)
  )

qlfs_model <- qlfs %>%
  filter(
    main_employment_sample_flag == 1,
    age >= 25,
    age <= 64
  ) %>%
  mutate(
    educ_group = factor(recode_educ_group(educ_group), levels = education_order),
    educ_group = fct_relevel(educ_group, "Incomplete secondary"),

    gender = factor(clean_label(gender)),
    population_group = factor(clean_label(population_group)),
    province = factor(clean_label(province)),
    geo_type = factor(clean_label(geo_type)),

    employed = as.numeric(employed),
    age = as.numeric(age),
    age_sq = as.numeric(age_sq),
    weight = as.numeric(weight),
    stratum = as.character(stratum)
  ) %>%
  filter(
    !is.na(employed),
    !is.na(educ_group),
    !is.na(age),
    !is.na(age_sq),
    !is.na(gender),
    !is.na(population_group),
    !is.na(province),
    !is.na(geo_type),
    !is.na(weight),
    weight > 0,
    !is.na(stratum)
  )

cat("\nLMDSA wage model sample rows:", nrow(lmdsa_model), "\n")
cat("QLFS employment model sample rows:", nrow(qlfs_model), "\n")

# ----------------------------
# Survey designs
# ----------------------------

lmdsa_design <- svydesign(
  ids = ~psu,
  strata = ~stratum,
  weights = ~weight,
  data = lmdsa_model,
  nest = TRUE
)

# QLFS cleaned data currently does not include PSU, so use strata + weights.
# This still gives weighted descriptive regressions, but standard errors
# should be interpreted cautiously relative to full survey design.
qlfs_design <- svydesign(
  ids = ~1,
  strata = ~stratum,
  weights = ~weight,
  data = qlfs_model,
  nest = TRUE
)

# ============================================================
# A. Wage models: LMDSA
# ============================================================

wage_m1 <- svyglm(
  log_employee_wage ~ educ_group + potential_experience + potential_experience_sq,
  design = lmdsa_design
)

wage_m2 <- svyglm(
  log_employee_wage ~ educ_group + potential_experience + potential_experience_sq +
    gender + population_group,
  design = lmdsa_design
)

wage_m3 <- svyglm(
  log_employee_wage ~ educ_group + potential_experience + potential_experience_sq +
    gender + population_group + province + geo_type,
  design = lmdsa_design
)

wage_m4 <- svyglm(
  log_employee_wage ~ educ_group + potential_experience + potential_experience_sq +
    gender + population_group + province + geo_type +
    occupation_group + industry_group + sector2 + hours_worked,
  design = lmdsa_design
)

# Interaction model for heterogeneity: education x population group.
# This is exploratory and should be interpreted cautiously.
wage_m5_interaction <- svyglm(
  log_employee_wage ~ educ_group * population_group +
    potential_experience + potential_experience_sq +
    gender + province + geo_type,
  design = lmdsa_design
)

wage_models <- list(
  wage_m1 = wage_m1,
  wage_m2 = wage_m2,
  wage_m3 = wage_m3,
  wage_m4 = wage_m4,
  wage_m5_interaction = wage_m5_interaction
)

saveRDS(wage_models, file.path(models_dir, "wage_models_lmdsa_2023.rds"))

wage_tidy <- bind_rows(
  tidy_svyglm(wage_m1, "W1: education + experience"),
  tidy_svyglm(wage_m2, "W2: + gender + population group"),
  tidy_svyglm(wage_m3, "W3: + province + geography"),
  tidy_svyglm(wage_m4, "W4: + job controls"),
  tidy_svyglm(wage_m5_interaction, "W5: education x population group")
)

write_table(wage_tidy, "01_wage_model_coefficients_all.csv")

wage_education_effects <- bind_rows(
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(wage_m1, "W1: education + experience"),
      outcome_type = "log_wage"
    ),
    "W1: education + experience",
    outcome_type = "log_wage"
  ),
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(wage_m2, "W2: + gender + population group"),
      outcome_type = "log_wage"
    ),
    "W2: + gender + population group",
    outcome_type = "log_wage"
  ),
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(wage_m3, "W3: + province + geography"),
      outcome_type = "log_wage"
    ),
    "W3: + province + geography",
    outcome_type = "log_wage"
  ),
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(wage_m4, "W4: + job controls"),
      outcome_type = "log_wage"
    ),
    "W4: + job controls",
    outcome_type = "log_wage"
  )
) %>%
  mutate(
    education_group = factor(education_group, levels = education_order)
  )

write_table(wage_education_effects, "02_wage_education_effects_all_models.csv")

wage_education_effects_report <- wage_education_effects %>%
  transmute(
    model,
    education_group,
    wage_premium_pct = round(effect, 1),
    conf_low_pct = round(effect_low, 1),
    conf_high_pct = round(effect_high, 1),
    p_value = format_p(p_value)
  )

write_table(wage_education_effects_report, "02b_wage_education_effects_report_ready.csv")

p_wage_effects <- wage_education_effects %>%
  filter(model %in% c(
    "W1: education + experience",
    "W3: + province + geography",
    "W4: + job controls"
  )) %>%
  ggplot(aes(x = education_group, y = effect, ymin = effect_low, ymax = effect_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~model) +
  labs(
    title = "Estimated wage premiums by education level",
    subtitle = "Relative to incomplete secondary; survey-weighted LMDSA models",
    x = "Education group",
    y = "Estimated wage premium (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_wage_effects, "01_wage_education_effects_all_models.png", width = 13, height = 7)

# ============================================================
# B. Employment models: QLFS
# ============================================================

employment_m1 <- svyglm(
  employed ~ educ_group + age + age_sq,
  design = qlfs_design
)

employment_m2 <- svyglm(
  employed ~ educ_group + age + age_sq +
    gender + population_group,
  design = qlfs_design
)

employment_m3 <- svyglm(
  employed ~ educ_group + age + age_sq +
    gender + population_group + province + geo_type,
  design = qlfs_design
)

# Exploratory heterogeneity model
employment_m4_interaction <- svyglm(
  employed ~ educ_group * population_group +
    age + age_sq + gender + province + geo_type,
  design = qlfs_design
)

employment_models <- list(
  employment_m1 = employment_m1,
  employment_m2 = employment_m2,
  employment_m3 = employment_m3,
  employment_m4_interaction = employment_m4_interaction
)

saveRDS(employment_models, file.path(models_dir, "employment_models_qlfs_2023.rds"))

employment_tidy <- bind_rows(
  tidy_svyglm(employment_m1, "E1: education + age"),
  tidy_svyglm(employment_m2, "E2: + gender + population group"),
  tidy_svyglm(employment_m3, "E3: + province + geography"),
  tidy_svyglm(employment_m4_interaction, "E4: education x population group")
)

write_table(employment_tidy, "03_employment_model_coefficients_all.csv")

employment_education_effects <- bind_rows(
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(employment_m1, "E1: education + age"),
      outcome_type = "lpm"
    ),
    "E1: education + age",
    outcome_type = "lpm"
  ),
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(employment_m2, "E2: + gender + population group"),
      outcome_type = "lpm"
    ),
    "E2: + gender + population group",
    outcome_type = "lpm"
  ),
  add_reference_row(
    extract_education_effects(
      tidy_svyglm(employment_m3, "E3: + province + geography"),
      outcome_type = "lpm"
    ),
    "E3: + province + geography",
    outcome_type = "lpm"
  )
) %>%
  mutate(
    education_group = factor(education_group, levels = education_order)
  )

write_table(employment_education_effects, "04_employment_education_effects_all_models.csv")

employment_education_effects_report <- employment_education_effects %>%
  transmute(
    model,
    education_group,
    employment_association_pp = round(effect, 1),
    conf_low_pp = round(effect_low, 1),
    conf_high_pp = round(effect_high, 1),
    p_value = format_p(p_value)
  )

write_table(employment_education_effects_report, "04b_employment_education_effects_report_ready.csv")

p_employment_effects <- employment_education_effects %>%
  ggplot(aes(x = education_group, y = effect, ymin = effect_low, ymax = effect_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~model) +
  labs(
    title = "Estimated employment associations by education level",
    subtitle = "Relative to incomplete secondary; survey-weighted QLFS linear probability models",
    x = "Education group",
    y = "Employment association (percentage points)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_employment_effects, "02_employment_education_effects_all_models.png", width = 13, height = 7)

# ============================================================
# C. Compact final model tables
# ============================================================

wage_final_report <- wage_education_effects_report %>%
  filter(model == "W4: + job controls") %>%
  select(
    education_group,
    wage_premium_pct,
    conf_low_pct,
    conf_high_pct,
    p_value
  )

write_table(wage_final_report, "05_final_wage_model_education_effects.csv")

employment_final_report <- employment_education_effects_report %>%
  filter(model == "E3: + province + geography") %>%
  select(
    education_group,
    employment_association_pp,
    conf_low_pp,
    conf_high_pp,
    p_value
  )

write_table(employment_final_report, "06_final_employment_model_education_effects.csv")

# ============================================================
# D. Console summary
# ============================================================

cat("\n================ MODELS COMPLETE ================\n")

cat("\nLMDSA wage model sample rows:\n")
print(nrow(lmdsa_model))

cat("\nQLFS employment model sample rows:\n")
print(nrow(qlfs_model))

cat("\nFinal wage model education effects, W4:\n")
print(wage_final_report)

cat("\nFinal employment model education effects, E3:\n")
print(employment_final_report)

cat("\nModel outputs saved to:", tables_dir, "\n")
cat("Model figures saved to:", figures_dir, "\n")
cat("Model objects saved to:", models_dir, "\n")
cat("=================================================\n")
