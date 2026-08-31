
# ============================================================
# 06_temporal_2025.R
#
# MSc Dissertation:
# Betting-Shop Concentration, Deprivation and Recorded Theft
# in London: A Spatial Crime Analysis
#
# Purpose:
# Conduct the temporal sensitivity analysis using 2025
# Police.uk theft data.
#
# The same four theft categories and the same principal
# Negative Binomial specification are used as for 2024.
#
# Input:
# - 2025 Police.uk street-level crime files
# - data/processed/final_london_lsoa_dataset_cleaned.csv
#
# Output:
# outputs/tables/temporal_2025_sensitivity.csv
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(MASS)


# ------------------------------------------------------------
# 2. Project paths
# ------------------------------------------------------------

crime_2025_dir <- file.path(
  "data",
  "raw",
  "crime_2025"
)

analysis_file <- file.path(
  "data",
  "processed",
  "final_london_lsoa_dataset_cleaned.csv"
)

output_dir <- file.path(
  "outputs",
  "tables"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. Load final analytical dataset
# ------------------------------------------------------------

analysis_data <- read_csv(
  analysis_file,
  show_col_types = FALSE
)


stopifnot(
  nrow(analysis_data) == 4994
)


analysis_data <- analysis_data %>%
  mutate(
    log_population =
      log(population)
  )


# ------------------------------------------------------------
# 4. Identify 2025 Police.uk street files
# ------------------------------------------------------------

all_2025_files <- list.files(
  crime_2025_dir,
  pattern = "-street\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


crime_files_2025 <- all_2025_files[
  grepl(
    paste0(
      "^2025-(0[1-9]|1[0-2])-",
      "(metropolitan|city-of-london)-street\\.csv$"
    ),
    basename(all_2025_files),
    ignore.case = TRUE
  )
]


cat(
  "\nNumber of 2025 crime files:",
  length(crime_files_2025),
  "\n"
)


# 12 months x 2 police forces
stopifnot(
  length(crime_files_2025) == 24
)


print(
  sort(
    basename(
      crime_files_2025
    )
  )
)


# ------------------------------------------------------------
# 5. Import 2025 crime data
# ------------------------------------------------------------

crime_2025_raw <- map_dfr(
  crime_files_2025,
  function(file_path) {

    read_csv(
      file_path,
      col_types = cols(.default = col_character()),
      progress = FALSE
    ) %>%

      transmute(
        crime_id =
          `Crime ID`,

        month =
          Month,

        reported_by =
          `Reported by`,

        crime_type =
          `Crime type`,

        lsoa_code =
          `LSOA code`,

        lsoa_name =
          `LSOA name`
      )
  }
)


# ------------------------------------------------------------
# 6. Check imported 2025 data
# ------------------------------------------------------------

raw_2025_check <- crime_2025_raw %>%
  summarise(

    raw_records =
      n(),

    months =
      n_distinct(month),

    police_forces =
      n_distinct(reported_by)
  )


print(
  raw_2025_check,
  width = Inf
)


crime_2025_raw %>%
  count(
    reported_by,
    name = "raw_records"
  ) %>%
  print(
    width = Inf
  )


# ------------------------------------------------------------
# 7. Select the four theft categories
# ------------------------------------------------------------

theft_types <- c(
  "Shoplifting",
  "Theft from the person",
  "Bicycle theft",
  "Other theft"
)


theft_2025 <- crime_2025_raw %>%

  filter(
    crime_type %in%
      theft_types
  )


cat(
  "\n2025 candidate theft records:",
  nrow(theft_2025),
  "\n"
)


theft_2025 %>%
  count(
    crime_type,
    sort = TRUE
  ) %>%
  print(
    n = Inf
  )


# ------------------------------------------------------------
# 8. Aggregate 2025 theft to LSOA
# ------------------------------------------------------------
#
# This follows the temporal sensitivity analysis used in the
# dissertation: valid LSOA-coded theft records are aggregated
# to the 2021 LSOA level.

theft_2025_lsoa <- theft_2025 %>%

  filter(
    !is.na(lsoa_code),
    lsoa_code != ""
  ) %>%

  count(
    lsoa_code,
    name = "theft_count_2025"
  )


cat(
  "\nLSOAs with recorded theft in 2025:",
  nrow(theft_2025_lsoa),
  "\n"
)

cat(
  "Total LSOA-coded theft records in 2025:",
  sum(
    theft_2025_lsoa$theft_count_2025
  ),
  "\n"
)


# ------------------------------------------------------------
# 9. Join 2025 theft counts to the analytical dataset
# ------------------------------------------------------------

analysis_2025 <- analysis_data %>%

  left_join(
    theft_2025_lsoa,
    by = "lsoa_code"
  ) %>%

  mutate(
    theft_count_2025 =
      replace_na(
        theft_count_2025,
        0L
      )
  )


stopifnot(
  nrow(analysis_2025) == 4994
)


# ------------------------------------------------------------
# 10. Check 2025 outcome
# ------------------------------------------------------------

summary_2025 <- analysis_2025 %>%

  summarise(

    number_of_lsoas =
      n(),

    total_theft_2025 =
      sum(
        theft_count_2025
      ),

    mean_theft_2025 =
      mean(
        theft_count_2025
      ),

    median_theft_2025 =
      median(
        theft_count_2025
      ),

    maximum_theft_2025 =
      max(
        theft_count_2025
      ),

    zero_theft_lsoas =
      sum(
        theft_count_2025 == 0
      )
  )


print(
  summary_2025,
  width = Inf
)


# ============================================================
# TEMPORAL SENSITIVITY MODEL
# ============================================================


# ------------------------------------------------------------
# 11. Estimate 2025 Negative Binomial model
# ------------------------------------------------------------
#
# Same specification as the principal 2024 Income Deprivation
# model, with only the theft outcome changed to 2025.

temporal_2025_nb <- MASS::glm.nb(

  theft_count_2025 ~
    betting_shop_count +
    income_score +
    population_density +
    offset(log_population),

  data = analysis_2025
)


summary(
  temporal_2025_nb
)


# ------------------------------------------------------------
# 12. Helper function for IRR and confidence interval
# ------------------------------------------------------------

extract_irr <- function(
  model,
  term,
  scale = 1
) {

  model_table <-
    coef(
      summary(model)
    )

  beta <-
    model_table[
      term,
      "Estimate"
    ]

  se <-
    model_table[
      term,
      "Std. Error"
    ]

  p_value <-
    model_table[
      term,
      "Pr(>|z|)"
    ]


  tibble(

    Variable =
      term,

    IRR =
      exp(
        beta * scale
      ),

    Lower_95_CI =
      exp(
        (
          beta -
            1.96 * se
        ) * scale
      ),

    Upper_95_CI =
      exp(
        (
          beta +
            1.96 * se
        ) * scale
      ),

    p_value =
      p_value
  )
}


# ------------------------------------------------------------
# 13. Extract 2025 model results
# ------------------------------------------------------------

temporal_results <- bind_rows(

  extract_irr(
    temporal_2025_nb,
    "betting_shop_count"
  ) %>%
    mutate(
      Variable =
        "Betting-shop count"
    ),

  extract_irr(
    temporal_2025_nb,
    "income_score",
    scale = 0.10
  ) %>%
    mutate(
      Variable =
        "Income deprivation score (+0.10)"
    ),

  extract_irr(
    temporal_2025_nb,
    "population_density",
    scale = 1000
  ) %>%
    mutate(
      Variable =
        "Population density (+1,000 residents/km2)"
    )
)


temporal_results <- temporal_results %>%

  mutate(

    N =
      nobs(
        temporal_2025_nb
      ),

    AIC =
      AIC(
        temporal_2025_nb
      ),

    Theta =
      temporal_2025_nb$theta,

    .before =
      Variable
  )


print(
  temporal_results,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 14. Save temporal sensitivity results
# ------------------------------------------------------------

write_csv(
  temporal_results,
  file.path(
    output_dir,
    "temporal_2025_sensitivity.csv"
  )
)


# ============================================================
# COMPARE 2024 AND 2025 BETTING-SHOP ESTIMATES
# ============================================================


# ------------------------------------------------------------
# 15. Re-estimate the 2024 principal model for comparison
# ------------------------------------------------------------

principal_2024_nb <- MASS::glm.nb(

  theft_count ~
    betting_shop_count +
    income_score +
    population_density +
    offset(log_population),

  data = analysis_data
)


# ------------------------------------------------------------
# 16. Extract betting-shop results from both years
# ------------------------------------------------------------

extract_betting_year <- function(
  model,
  year
) {

  model_table <-
    coef(
      summary(model)
    )

  beta <-
    model_table[
      "betting_shop_count",
      "Estimate"
    ]

  se <-
    model_table[
      "betting_shop_count",
      "Std. Error"
    ]

  p <-
    model_table[
      "betting_shop_count",
      "Pr(>|z|)"
    ]


  tibble(

    Year =
      year,

    N =
      nobs(model),

    Betting_shop_IRR =
      exp(beta),

    Lower_95_CI =
      exp(
        beta -
          1.96 * se
      ),

    Upper_95_CI =
      exp(
        beta +
          1.96 * se
      ),

    p_value =
      p,

    AIC =
      AIC(model)
  )
}


temporal_comparison <- bind_rows(

  extract_betting_year(
    principal_2024_nb,
    "2024"
  ),

  extract_betting_year(
    temporal_2025_nb,
    "2025"
  )
)


print(
  temporal_comparison,
  width = Inf
)


write_csv(
  temporal_comparison,
  file.path(
    output_dir,
    "temporal_2024_2025_comparison.csv"
  )
)


# ------------------------------------------------------------
# 17. Validate principal estimates
# ------------------------------------------------------------

irr_2024 <-
  exp(
    coef(
      principal_2024_nb
    )[
      "betting_shop_count"
    ]
  )


irr_2025 <-
  exp(
    coef(
      temporal_2025_nb
    )[
      "betting_shop_count"
    ]
  )


cat(
  "\n2024 betting-shop IRR:",
  round(
    irr_2024,
    3
  ),
  "\n"
)

cat(
  "2025 betting-shop IRR:",
  round(
    irr_2025,
    3
  ),
  "\n"
)


# Expected approximate dissertation results:
#
# 2024:
# IRR = 2.087
#
# 2025:
# IRR = 2.058
# 95% CI approximately 1.964-2.156
# p < .001

stopifnot(
  round(
    irr_2024,
    3
  ) == 2.087
)

stopifnot(
  round(
    irr_2025,
    3
  ) == 2.058
)


# ------------------------------------------------------------
# 18. Completion
# ------------------------------------------------------------

message(
  "06_temporal_2025.R completed successfully."
)
