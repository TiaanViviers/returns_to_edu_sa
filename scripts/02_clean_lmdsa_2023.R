suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))

input_path <- "data/raw/lmdsa_2023/data/LMD2023.csv"
output_path <- "data/processed/lmdsa_2023/lmdsa_2023_clean.csv"

if (!file.exists(input_path)) {
  stop(paste("Input file is missing:", input_path), call. = FALSE)
}

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

lmd_col_types <- cols(
  LMD2023_Uqno = col_character(),
  LMD2023_Personno = col_character(),
  LMD2023_Q27Atime = col_double(),
  LMD2023_Q27Brecpay = col_double(),
  LMD2023_Q311Rsnnotavailable = col_double(),
  LMD2023_Q311Bwhnstart = col_double(),
  LMD2023_Q59Etime = col_double()
)

lmd <- read_csv(
  input_path,
  col_types = lmd_col_types,
  show_col_types = FALSE
)

lmd_clean <- lmd %>%
  mutate(
    person_id = paste(LMD2023_Uqno, LMD2023_Personno, sep = "_"),

    quarter = LMD2023_Qtr,

    age = LMD2023_Q14Age,
    age_sq = age^2,

    gender = case_when(
      LMD2023_Q13Gender == 1 ~ "male",
      LMD2023_Q13Gender == 2 ~ "female",
      TRUE ~ NA_character_
    ),

    female = case_when(
      LMD2023_Q13Gender == 2 ~ 1L,
      LMD2023_Q13Gender == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),

    population_group = case_when(
      LMD2023_Q15Population == 1 ~ "black_african",
      LMD2023_Q15Population == 2 ~ "coloured",
      LMD2023_Q15Population == 3 ~ "indian_asian",
      LMD2023_Q15Population == 4 ~ "white",
      TRUE ~ NA_character_
    ),

    province = case_when(
      LMD2023_Province == 1 ~ "western_cape",
      LMD2023_Province == 2 ~ "eastern_cape",
      LMD2023_Province == 3 ~ "northern_cape",
      LMD2023_Province == 4 ~ "free_state",
      LMD2023_Province == 5 ~ "kwazulu_natal",
      LMD2023_Province == 6 ~ "north_west",
      LMD2023_Province == 7 ~ "gauteng",
      LMD2023_Province == 8 ~ "mpumalanga",
      LMD2023_Province == 9 ~ "limpopo",
      TRUE ~ NA_character_
    ),

    geo_type = case_when(
      LMD2023_Geo_Type == 1 ~ "urban_formal",
      LMD2023_Geo_Type == 2 ~ "urban_informal",
      LMD2023_Geo_Type == 3 ~ "tribal_area",
      LMD2023_Geo_Type == 4 ~ "rural_formal",
      TRUE ~ NA_character_
    ),

    education_raw = LMD2023_Q17Education,
    education_status_raw = LMD2023_Education_Status,
    graduates_raw = LMD2023_Graduates,

    educ_group = case_when(
      education_raw == 98 ~ "no_schooling",
      education_raw %in% 0:6 ~ "less_than_primary",
      education_raw == 7 ~ "primary",
      education_raw %in% 8:11 ~ "incomplete_secondary",
      education_raw == 12 ~ "matric",
      education_raw %in% 13:24 ~ "certificate_diploma",
      education_raw %in% 25:28 ~ "degree_postgrad",
      TRUE ~ NA_character_
    ),

    educ_years_approx = case_when(
      education_raw == 98 ~ 0,
      education_raw == 0 ~ 0,
      education_raw %in% 1:12 ~ as.numeric(education_raw),
      education_raw %in% c(13, 14, 15) ~ 10,
      education_raw %in% c(16, 17, 18) ~ 12,
      education_raw %in% c(19, 20) ~ 10,
      education_raw %in% c(21, 22) ~ 13,
      education_raw == 23 ~ 14,
      education_raw == 24 ~ 15,
      education_raw == 25 ~ 15,
      education_raw == 26 ~ 16,
      education_raw == 27 ~ 16,
      education_raw == 28 ~ 18,
      TRUE ~ NA_real_
    ),

    potential_experience = age - educ_years_approx - 6,
    potential_experience = if_else(potential_experience < 0, NA_real_, potential_experience),
    potential_experience_sq = potential_experience^2,

    employment_status = case_when(
      LMD2023_Status == 1 ~ "employed",
      LMD2023_Status == 2 ~ "unemployed",
      LMD2023_Status == 3 ~ "discouraged",
      LMD2023_Status == 4 ~ "other_not_economically_active",
      TRUE ~ NA_character_
    ),

    employed = as.integer(LMD2023_Status == 1),
    unemployed = as.integer(LMD2023_Status == 2),
    discouraged = as.integer(LMD2023_Status == 3),

    occupation_raw = LMD2023_Q42Occupation,
    occupation_group = LMD2023_Occup,

    industry_raw = LMD2023_Q43Industry,
    industry_group = LMD2023_Indus,

    sector1 = LMD2023_Sector1,
    sector2 = LMD2023_Sector2,
    informal_employment = LMD2023_Infempl,

    hours_worked = LMD2023_Hrswrk,

    employee_salary_interval = LMD2023_Q52Salaryinterval,
    employee_monthly_wage = if_else(
      !is.na(LMD2023_Q54a_Monthly) & LMD2023_Q54a_Monthly > 0,
      as.numeric(LMD2023_Q54a_Monthly),
      NA_real_
    ),

    self_employed_monthly_earnings = if_else(
      !is.na(LMD2023_Q57a_Monthly) & LMD2023_Q57a_Monthly > 0,
      as.numeric(LMD2023_Q57a_Monthly),
      NA_real_
    ),

    monthly_earnings_combined = if_else(
      !is.na(LMD2023_Monthly_Amount) & LMD2023_Monthly_Amount > 0,
      as.numeric(LMD2023_Monthly_Amount),
      NA_real_
    ),

    log_employee_wage = log(employee_monthly_wage),
    log_monthly_earnings_combined = log(monthly_earnings_combined),

    wage_sample_flag = if_else(
      !is.na(age) &
        age >= 25 &
        age <= 64 &
        employed == 1 &
        employee_salary_interval %in% 1:6 &
        !is.na(employee_monthly_wage) &
        !is.na(hours_worked) &
        hours_worked >= 10 &
        hours_worked <= 84 &
        !is.na(educ_group) &
        !is.na(potential_experience),
      1L,
      0L
    ),

    wage_sample_trimmed_flag = if_else(
      wage_sample_flag == 1 &
        employee_monthly_wage >= 500 &
        employee_monthly_wage <= 300000,
      1L,
      0L
    ),

    weight = LMD2023_Weight,
    weight_qtr = LMD2023_Weight_Qtr,
    stratum = LMD2023_Stratum,
    psu = LMD2023_Psuno_Seg
  ) %>%
  select(
    person_id,
    quarter,
    age,
    age_sq,
    gender,
    female,
    population_group,
    province,
    LMD2023_Metro_Code,
    geo_type,
    education_raw,
    education_status_raw,
    graduates_raw,
    educ_group,
    educ_years_approx,
    potential_experience,
    potential_experience_sq,
    employment_status,
    employed,
    unemployed,
    discouraged,
    occupation_raw,
    occupation_group,
    industry_raw,
    industry_group,
    sector1,
    sector2,
    informal_employment,
    hours_worked,
    employee_salary_interval,
    employee_monthly_wage,
    self_employed_monthly_earnings,
    monthly_earnings_combined,
    log_employee_wage,
    log_monthly_earnings_combined,
    wage_sample_flag,
    wage_sample_trimmed_flag,
    weight,
    weight_qtr,
    stratum,
    psu
  )

write_csv(lmd_clean, output_path)

cat("\nSaved cleaned LMDSA data to:", output_path, "\n")
cat("\nRows:", nrow(lmd_clean), "\n")
cat("Columns:", ncol(lmd_clean), "\n")

cat("\nWage sample count:\n")
print(table(lmd_clean$wage_sample_flag, useNA = "ifany"))

cat("\nEducation groups in wage sample:\n")
print(table(lmd_clean$educ_group[lmd_clean$wage_sample_flag == 1], useNA = "ifany"))

cat("\nEmployee wage summary in wage sample:\n")
print(summary(lmd_clean$employee_monthly_wage[lmd_clean$wage_sample_flag == 1]))

cat("\nTrimmed wage sample count:\n")
print(table(lmd_clean$wage_sample_trimmed_flag, useNA = "ifany"))

cat("\nEmployee wage summary in trimmed wage sample:\n")
print(summary(lmd_clean$employee_monthly_wage[lmd_clean$wage_sample_trimmed_flag == 1]))

removed_n <- sum(lmd_clean$wage_sample_flag == 1) -
  sum(lmd_clean$wage_sample_trimmed_flag == 1)

removed_pct <- removed_n / sum(lmd_clean$wage_sample_flag == 1) * 100

cat("\nTrimmed observations removed from main wage sample:\n")
cat(removed_n, "observations removed (", round(removed_pct, 2), "% )\n")

cat("\nLowest 20 employee wages in main wage sample:\n")
print(
  lmd_clean %>%
    filter(wage_sample_flag == 1) %>%
    arrange(employee_monthly_wage) %>%
    select(
      age,
      educ_group,
      hours_worked,
      employee_monthly_wage,
      employee_salary_interval,
      occupation_group,
      industry_group
    ) %>%
    head(20)
)

cat("\nHighest 20 employee wages in main wage sample:\n")
print(
  lmd_clean %>%
    filter(wage_sample_flag == 1) %>%
    arrange(desc(employee_monthly_wage)) %>%
    select(
      age,
      educ_group,
      hours_worked,
      employee_monthly_wage,
      employee_salary_interval,
      occupation_group,
      industry_group
    ) %>%
    head(20)
)
