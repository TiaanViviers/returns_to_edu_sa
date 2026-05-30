suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))

quarter_paths <- c(
  q1 = "data/raw/qlfs_2023/q1/data/QLFS202301.csv",
  q2 = "data/raw/qlfs_2023/q2/data/QLFS202302.csv",
  q3 = "data/raw/qlfs_2023/q3/data/QLFS202303.csv",
  q4 = "data/raw/qlfs_2023/q4/data/QLFS202304.csv"
)

output_path <- "data/processed/qlfs_2023/qlfs_2023_clean.csv"

qlfs_col_types <- cols(
  UQNO = col_character(),
  PERSONNO = col_character(),
  Q27ATIME = col_double(),
  Q27BRECPAY = col_double(),
  Q33AWHNSTART = col_double(),
  Q311RSNNOTAVAILABLE = col_double(),
  Q311bWHNSTART = col_double(),
  Q420FIRSTHRSWRK = col_double(),
  Q420SECONDHRSWRK = col_double(),
  Q420OTHERHRSWRK = col_double(),
  Q420TOTALHRSWRK = col_double(),
  Q4211MONHRSWRK = col_double(),
  Q4211TUEHRSWRK = col_double(),
  Q4211WEDHRSWRK = col_double(),
  Q4211THUHRSWRK = col_double(),
  Q4211FRIHRSWRK = col_double(),
  Q4211SATHRSWRK = col_double(),
  Q4211SUNHRSWRK = col_double(),
  Q4211TOTALHRS = col_double(),
  Q4212MONHRSWRK = col_double(),
  Q4212TUEHRSWRK = col_double(),
  Q4212WEDHRSWRK = col_double(),
  Q4212THUHRSWRK = col_double(),
  Q4212FRIHRSWRK = col_double(),
  Q4212SATHRSWRK = col_double(),
  Q4212SUNHRSWRK = col_double(),
  Q4212TOTALHRS = col_double(),
  Q4213MONHRSWRK = col_double(),
  Q4213TUEHRSWRK = col_double(),
  Q4213WEDHRSWRK = col_double(),
  Q4213THUHRSWRK = col_double(),
  Q4213FRIHRSWRK = col_double(),
  Q4213SATHRSWRK = col_double(),
  Q4213SUNHRSWRK = col_double(),
  Q4213TOTALHRS = col_double(),
  Q59ETIME = col_double()
)

missing_files <- unname(quarter_paths)[!file.exists(unname(quarter_paths))]

if (length(missing_files) > 0) {
  stop(
    paste(
      "The following input files are missing:",
      paste(missing_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

read_quarter <- function(path, quarter_number) {
  read_csv(
    path,
    col_types = qlfs_col_types,
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  ) %>%
    select(-starts_with("...")) %>%
    mutate(quarter = quarter_number)
}

qlfs_raw <- map2_dfr(
  .x = quarter_paths,
  .y = seq_along(quarter_paths),
  .f = read_quarter
)

qlfs_clean <- qlfs_raw %>%
  mutate(
    person_id = paste(UQNO, PERSONNO, sep = "_"),

    age = Q14AGE,
    age_sq = age^2,

    gender = case_when(
      Q13GENDER == 1 ~ "male",
      Q13GENDER == 2 ~ "female",
      TRUE ~ NA_character_
    ),

    female = case_when(
      Q13GENDER == 2 ~ 1L,
      Q13GENDER == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),

    population_group = case_when(
      Q15POPULATION == 1 ~ "black_african",
      Q15POPULATION == 2 ~ "coloured",
      Q15POPULATION == 3 ~ "indian_asian",
      Q15POPULATION == 4 ~ "white",
      TRUE ~ NA_character_
    ),

    province = case_when(
      Province == 1 ~ "western_cape",
      Province == 2 ~ "eastern_cape",
      Province == 3 ~ "northern_cape",
      Province == 4 ~ "free_state",
      Province == 5 ~ "kwazulu_natal",
      Province == 6 ~ "north_west",
      Province == 7 ~ "gauteng",
      Province == 8 ~ "mpumalanga",
      Province == 9 ~ "limpopo",
      TRUE ~ NA_character_
    ),

    geo_type = case_when(
      Geo_type_code == 1 ~ "urban",
      Geo_type_code == 2 ~ "traditional",
      Geo_type_code == 3 ~ "farms",
      TRUE ~ NA_character_
    ),

    education_raw = Q17EDUCATION,
    education_status_raw = Education_Status,

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

    employment_status = case_when(
      Status == 1 ~ "employed",
      Status == 2 ~ "unemployed",
      Status == 3 ~ "discouraged",
      Status == 4 ~ "other_not_economically_active",
      TRUE ~ NA_character_
    ),

    employed = if_else(!is.na(Status) & Status == 1, 1L, 0L),
    unemployed = if_else(!is.na(Status) & Status == 2, 1L, 0L),
    discouraged = if_else(!is.na(Status) & Status == 3, 1L, 0L),
    not_economically_active = if_else(!is.na(Status) & Status == 4, 1L, 0L),

    labour_force_flag = if_else(!is.na(Status) & Status %in% c(1, 2), 1L, 0L),
    expanded_labour_force_flag = if_else(!is.na(Status) & Status %in% c(1, 2, 3), 1L, 0L),

    occupation_raw = Q42OCCUPATION,
    occupation_group = Occup,

    industry_raw = Q43INDUSTRY,
    industry_group = Indus,

    sector1 = sector1,
    sector2 = Sector2,
    informal_employment = Infempl,

    hours_worked = Hrswrk,

    neet = NEET,

    working_age_flag = if_else(!is.na(age) & age >= 15 & age <= 64, 1L, 0L),

    main_employment_sample_flag = if_else(
      !is.na(age) &
        age >= 15 &
        age <= 64 &
        !is.na(educ_group) &
        !is.na(employment_status),
      1L,
      0L
    ),

    weight = Weight,
    stratum = STRATUM
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
    Metro_code,
    geo_type,
    education_raw,
    education_status_raw,
    educ_group,
    educ_years_approx,
    employment_status,
    employed,
    unemployed,
    discouraged,
    not_economically_active,
    labour_force_flag,
    expanded_labour_force_flag,
    occupation_raw,
    occupation_group,
    industry_raw,
    industry_group,
    sector1,
    sector2,
    informal_employment,
    hours_worked,
    neet,
    working_age_flag,
    main_employment_sample_flag,
    weight,
    stratum
  )

write_csv(qlfs_clean, output_path)

cat("\nSaved cleaned QLFS data to:", output_path, "\n")
cat("\nRows:", nrow(qlfs_clean), "\n")
cat("Columns:", ncol(qlfs_clean), "\n")

cat("\nMain employment sample count:\n")
print(table(qlfs_clean$main_employment_sample_flag, useNA = "ifany"))

cat("\nEmployment status in main employment sample:\n")
print(table(qlfs_clean$employment_status[qlfs_clean$main_employment_sample_flag == 1], useNA = "ifany"))

cat("\nEducation groups in main employment sample:\n")
print(table(qlfs_clean$educ_group[qlfs_clean$main_employment_sample_flag == 1], useNA = "ifany"))
