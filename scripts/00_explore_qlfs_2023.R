suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

sample_size <- 1000

quarter_paths <- c(
  q1 = "data/raw/qlfs_2023/q1/data/QLFS202301.csv",
  q2 = "data/raw/qlfs_2023/q2/data/QLFS202302.csv",
  q3 = "data/raw/qlfs_2023/q3/data/QLFS202303.csv",
  q4 = "data/raw/qlfs_2023/q4/data/QLFS202304.csv"
)

required_paths <- unname(quarter_paths)
missing_files <- required_paths[!file.exists(required_paths)]

if (length(missing_files) > 0) {
  stop(
    paste(
      "The following input files are missing:",
      paste(missing_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

read_sample <- function(path, n_max = sample_size) {
  read_csv(path, n_max = n_max, show_col_types = FALSE, name_repair = "unique_quiet")
}

quarter_samples <- map(quarter_paths, read_sample)
quarter_columns <- map(quarter_samples, names)

cat("\nQ1 dimensions:\n")
print(dim(quarter_samples$q1))

cat("\nQ1 column names:\n")
print(quarter_columns$q1)

comparison_pairs <- c("q2", "q3", "q4")

for (quarter in comparison_pairs) {
  cat("\nColumns in Q1 not in", toupper(quarter), ":\n")
  print(setdiff(quarter_columns$q1, quarter_columns[[quarter]]))

  cat("\nColumns in", toupper(quarter), "not in Q1:\n")
  print(setdiff(quarter_columns[[quarter]], quarter_columns$q1))
}

patterns <- c(
  "educ", "school", "qual", "grade",
  "earn", "income", "wage", "salary", "pay", "amount",
  "employ", "labour", "work", "status",
  "age", "sex", "gender", "race", "population",
  "province", "metro", "urban", "geo",
  "industry", "occupation", "sector",
  "weight", "stratum"
)

for (pattern in patterns) {
  cat("\n--- Matching pattern:", pattern, "---\n")
  print(quarter_columns$q1[str_detect(tolower(quarter_columns$q1), pattern)])
}

dir.create("eda/tables", showWarnings = FALSE, recursive = TRUE)

column_info <- tibble(
  column_name = quarter_columns$q1,
  class = map_chr(quarter_samples$q1, ~ class(.x)[1]),
  n_missing_sample = map_int(quarter_samples$q1, ~ sum(is.na(.x))),
  n_unique_sample = map_int(quarter_samples$q1, n_distinct)
)

all_column_names <- sort(unique(unlist(quarter_columns)))

column_comparison <- tibble(
  column_name = all_column_names,
  in_q1 = column_name %in% quarter_columns$q1,
  in_q2 = column_name %in% quarter_columns$q2,
  in_q3 = column_name %in% quarter_columns$q3,
  in_q4 = column_name %in% quarter_columns$q4
)

output_path <- "eda/tables/qlfs_2023_q1_column_info_sample.csv"
write_csv(column_info, output_path)

comparison_output_path <- "eda/tables/qlfs_2023_column_comparison.csv"
write_csv(column_comparison, comparison_output_path)

cat("\nSaved column summary to", output_path, "\n")
cat("\nSaved column comparison to", comparison_output_path, "\n")
