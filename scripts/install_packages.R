required_packages <- c(
  "readr",
  "stringr",
  "tibble",
  "purrr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "forcats",
  "scales",
  "survey",
  "knitr",
  "rmarkdown"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) == 0) {
  cat("All required packages are already installed.\n")
} else {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
