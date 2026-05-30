suppressPackageStartupMessages(library(rmarkdown))

input_file <- "report/returns_to_education_report.Rmd"
output_dir <- "report"
output_file <- "returns_to_education_report.html"

if (!file.exists(input_file)) {
  stop(paste("Report file not found:", input_file), call. = FALSE)
}

Sys.setenv(PROJECT_ROOT = normalizePath(getwd(), winslash = "/", mustWork = TRUE))

rmarkdown::render(
  input = input_file,
  output_file = output_file,
  output_dir = output_dir,
  knit_root_dir = getwd(),
  clean = TRUE
)

cat("\nRendered report to:", file.path(output_dir, output_file), "\n")
