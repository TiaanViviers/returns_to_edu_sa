suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

lmdsa_path <- "data/raw/lmdsa_2023/data/LMD2023.csv"

if (!file.exists(lmdsa_path)) {
  stop(
    paste("Input file is missing:", lmdsa_path),
    call. = FALSE
  )
}

lmdsa_sample <- read_csv(lmdsa_path, n_max = 1000, show_col_types = FALSE)

cat("\nLMDSA dimensions, sample:\n")
print(dim(lmdsa_sample))

cat("\nLMDSA column names:\n")
print(names(lmdsa_sample))

patterns <- c(
  "educ", "school", "qual", "grade",
  "earn", "income", "wage", "salary", "pay", "amount", "monthly", "annual",
  "employ", "labour", "work", "status",
  "age", "sex", "gender", "race", "population",
  "province", "metro", "urban", "geo",
  "industry", "occupation", "sector",
  "hours", "hrs",
  "weight", "stratum"
)

column_names <- names(lmdsa_sample)

for (pattern in patterns) {
  cat("\n--- Matching pattern:", pattern, "---\n")
  print(column_names[str_detect(tolower(column_names), pattern)])
}

dir.create("eda/tables", showWarnings = FALSE, recursive = TRUE)

column_info <- tibble(
  column_name = column_names,
  class = map_chr(lmdsa_sample, ~ class(.x)[1]),
  n_missing_sample = map_int(lmdsa_sample, ~ sum(is.na(.x))),
  n_unique_sample = map_int(lmdsa_sample, n_distinct)
)

output_path <- "eda/tables/lmdsa_2023_column_info_sample.csv"
write_csv(column_info, output_path)

cat("\nSaved column summary to", output_path, "\n")