# Returns to Education in South Africa

Survey-weighted statistical analysis of how education is associated with employment and wages in South Africa using `QLFS 2023` and `LMDSA 2023`.

This repository is structured as a portfolio-quality econometrics project: it includes the data-cleaning pipeline, exploratory analysis, model estimation scripts, report-ready outputs, and a fully rendered HTML report.

## Research question

How strongly is education associated with labour-market outcomes in South Africa, and do returns appear to rise smoothly with schooling or cluster around credential thresholds such as matric and tertiary qualifications?

## Main findings

- Employment rises sharply at key credentials. Among adults aged `25-64`, the weighted employment rate increases from `41.7%` for incomplete secondary education to `53.4%` for matric, `64.3%` for certificate/diploma, and `80.9%` for degree/postgraduate education.
- Wages are even more non-linear. In the trimmed LMDSA employee sample, weighted median monthly wages rise from `R3,500` for incomplete secondary education to `R5,500` for matric, `R16,000` for certificate/diploma, and `R25,000` for degree/postgraduate education.
- The adjusted wage gradient remains large after controls. In the preferred survey-weighted log-wage model, relative to incomplete secondary education, matric is associated with a `46.4%` wage premium, certificate/diploma with `130.3%`, and degree/postgraduate education with `237.5%`.
- Education is also strongly associated with employment access. In the preferred employment model, matric is associated with an `11.1` percentage-point higher employment probability, certificate/diploma with `21.3` points, and degree/postgraduate education with `36.2` points.

## Data

The analysis combines two Statistics South Africa datasets from `2023`:

- `QLFS 2023` for employment outcomes.
- `LMDSA 2023` for wage outcomes.

Main analytical samples:

- `QLFS 2023 cleaned`: `264,495` rows.
- `QLFS employment EDA sample`: `165,997` rows.
- `LMDSA 2023 cleaned`: `264,495` rows.
- `LMDSA trimmed wage sample`: `45,726` rows.

For public GitHub publishing, raw microdata and large cleaned data files are kept out of version control. The repository still preserves the full code path and the lighter-weight outputs needed to inspect the analysis.

## Econometric approach

### Wage model

The wage analysis uses a survey-weighted Mincer-style log-wage regression on employees aged `25-64`:

```text
log(wage) ~ education + experience + experience^2 + demographics
            + province + geography + occupation + industry + sector + hours
```

Key design choices:

- Outcome: `log(employee_monthly_wage)`.
- Reference education group: `Incomplete secondary`.
- Preferred sample: employees aged `25-64` with positive wages, plausible hours, and trimmed monthly wages between `R500` and `R300,000`.
- Controls: potential experience, gender, population group, province, geography type, occupation, industry, sector, and hours worked.
- Estimator: `survey::svyglm`.

### Employment model

The employment analysis uses a survey-weighted linear probability model on adults aged `25-64`:

```text
employed ~ education + age + age^2 + gender + population_group + province + geography
```

Notes:

- Education is modeled as grouped credentials to capture non-linear returns.
- QLFS cleaned data currently preserve survey strata and weights but not PSU identifiers, so employment-model standard errors should be interpreted more cautiously than the LMDSA wage-model standard errors.
- All estimates in this project are interpreted as conditional associations, not causal effects.

## Repository structure

- `scripts/`
  Reproducible pipeline scripts for exploration, cleaning, EDA, modeling, and report rendering.
- `eda/tables/` and `eda/figures/`
  Descriptive outputs used in the report.
- `report/tables/` and `report/figures/`
  Model outputs and final comparison figures.
- `report/returns_to_education_report.Rmd`
  Source for the final narrative report.
- `report/returns_to_education_report.html`
  Rendered report artifact for quick review.
- `data/raw/README.md`
  Expected local raw-data layout.
- `data/processed/README.md`
  Notes on generated cleaned files.

## Reproducing the analysis

Install packages:

```bash
Rscript scripts/install_packages.R
```

Run the pipeline:

```bash
Rscript scripts/00_explore_qlfs_2023.R
Rscript scripts/00_explore_lmdsa_2023.R
Rscript scripts/01_clean_qlfs_2023.R
Rscript scripts/02_clean_lmdsa_2023.R
Rscript scripts/03_eda.R
Rscript scripts/04_models.R
Rscript scripts/05_render_report.R
```

If the cleaned data and intermediate outputs already exist locally, the report can be regenerated directly with:

```bash
Rscript scripts/05_render_report.R
```

## Key outputs

- Final report: `report/returns_to_education_report.html`
- Final wage effects table: `report/tables/05_final_wage_model_education_effects.csv`
- Final employment effects table: `report/tables/06_final_employment_model_education_effects.csv`
- Core model script: `scripts/04_models.R`
- Core descriptive script: `scripts/03_eda.R`
