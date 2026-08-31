
# ============================================================
# 04_main_models.R
#
# MSc Dissertation:
# Betting-Shop Concentration, Deprivation and Recorded Theft
# in London: A Spatial Crime Analysis
#
# Purpose:
# Estimate the principal and supplementary count models used
# in Chapter 4.
#
# Models:
# 1. Poisson models for overdispersion assessment
# 2. Principal Negative Binomial model using Income Deprivation
# 3. Supplementary Negative Binomial model using overall IMD
#
# Input:
# data/processed/final_london_lsoa_dataset_cleaned.csv
#
# Outputs:
# outputs/tables/table_4_4_model_diagnostics.csv
# outputs/tables/table_4_5_income_nb.csv
# outputs/tables/table_4_6_imd_nb.csv
# outputs/tables/main_model_fit_statistics.csv
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readr)
library(dplyr)
library(MASS)


# ------------------------------------------------------------
# 2. Project paths
# ------------------------------------------------------------

data_file <- file.path(
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
  data_file,
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 4. Validate analytical data
# ------------------------------------------------------------

required_variables <- c(
  "theft_count",
  "betting_shop_count",
  "income_score",
  "imd_score",
  "population_density",
  "population"
)

missing_variables <- setdiff(
  required_variables,
  names(analysis_data)
)

if (length(missing_variables) > 0) {
  stop(
    paste(
      "Missing required variables:",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  )
}


stopifnot(
  nrow(analysis_data) == 4994
)

stopifnot(
  sum(
    analysis_data$theft_count,
    na.rm = TRUE
  ) == 310108
)

stopifnot(
  all(
    analysis_data$population > 0,
    na.rm = TRUE
  )
)


analysis_data <- analysis_data %>%
  mutate(
    log_population =
      log(population)
  )


# ============================================================
# PART A. POISSON MODELS AND OVERDISPERSION
# ============================================================


# ------------------------------------------------------------
# 5. Principal Income Deprivation Poisson model
# ------------------------------------------------------------

poisson_income <- glm(
  theft_count ~
    betting_shop_count +
    income_score +
    population_density +
    offset(log_population),

  family = poisson(
    link = "log"
  ),

  data = analysis_data
)


# ------------------------------------------------------------
# 6. Supplementary IMD Poisson model
# ------------------------------------------------------------

poisson_imd <- glm(
  theft_count ~
    betting_shop_count +
    imd_score +
    population_density +
    offset(log_population),

  family = poisson(
    link = "log"
  ),

  data = analysis_data
)


# ------------------------------------------------------------
# 7. Pearson dispersion statistic
# ------------------------------------------------------------

pearson_dispersion <- function(model) {

  sum(
    residuals(
      model,
      type = "pearson"
    )^2
  ) /
    df.residual(model)
}


income_poisson_dispersion <-
  pearson_dispersion(
    poisson_income
  )

imd_poisson_dispersion <-
  pearson_dispersion(
    poisson_imd
  )


cat(
  "\nIncome Poisson dispersion:",
  income_poisson_dispersion,
  "\n"
)

cat(
  "IMD Poisson dispersion:",
  imd_poisson_dispersion,
  "\n"
)


# Very large values indicate substantial overdispersion and
# support the use of Negative Binomial regression.


# ============================================================
# PART B. NEGATIVE BINOMIAL MODELS
# ============================================================


# ------------------------------------------------------------
# 8. Principal Income Deprivation model
# ------------------------------------------------------------

nb_income <- MASS::glm.nb(

  theft_count ~
    betting_shop_count +
    income_score +
    population_density +
    offset(log_population),

  data = analysis_data
)


summary(
  nb_income
)


# ------------------------------------------------------------
# 9. Supplementary overall IMD model
# ------------------------------------------------------------

nb_imd <- MASS::glm.nb(

  theft_count ~
    betting_shop_count +
    imd_score +
    population_density +
    offset(log_population),

  data = analysis_data
)


summary(
  nb_imd
)


# ============================================================
# PART C. MODEL DIAGNOSTICS
# ============================================================


# ------------------------------------------------------------
# 10. Compare Poisson and Negative Binomial fit
# ------------------------------------------------------------

table_4_4 <- tibble(

  Model = c(
    "Income Poisson",
    "Income Negative Binomial",
    "IMD Poisson",
    "IMD Negative Binomial"
  ),

  N = c(
    nobs(poisson_income),
    nobs(nb_income),
    nobs(poisson_imd),
    nobs(nb_imd)
  ),

  AIC = c(
    AIC(poisson_income),
    AIC(nb_income),
    AIC(poisson_imd),
    AIC(nb_imd)
  ),

  Pearson_dispersion = c(
    income_poisson_dispersion,
    NA_real_,
    imd_poisson_dispersion,
    NA_real_
  )
)


print(
  table_4_4,
  n = Inf,
  width = Inf
)


write_csv(
  table_4_4,
  file.path(
    output_dir,
    "table_4_4_model_diagnostics.csv"
  )
)


# ============================================================
# PART D. IRR AND 95% CONFIDENCE INTERVALS
# ============================================================


# ------------------------------------------------------------
# 11. Helper function for scaled IRRs
# ------------------------------------------------------------

extract_irr <- function(
  model,
  term,
  scale = 1,
  label = term
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

  standard_error <-
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

    Variable = label,

    IRR =
      exp(
        beta * scale
      ),

    Lower_95_CI =
      exp(
        (
          beta -
            1.96 *
            standard_error
        ) *
          scale
      ),

    Upper_95_CI =
      exp(
        (
          beta +
            1.96 *
            standard_error
        ) *
          scale
      ),

    p_value =
      p_value
  )
}


# ============================================================
# TABLE 4.5
# PRINCIPAL INCOME DEPRIVATION MODEL
# ============================================================


# ------------------------------------------------------------
# 12. Income model results
# ------------------------------------------------------------

table_4_5 <- bind_rows(

  extract_irr(
    nb_income,
    term = "betting_shop_count",
    scale = 1,
    label = "Betting-shop count"
  ),

  # Report Income Deprivation for a 0.10 increase
  extract_irr(
    nb_income,
    term = "income_score",
    scale = 0.10,
    label = "Income deprivation score (+0.10)"
  ),

  # Report population density for +1,000 residents/km2
  extract_irr(
    nb_income,
    term = "population_density",
    scale = 1000,
    label = "Population density (+1,000 residents/km2)"
  )
)


print(
  table_4_5,
  n = Inf,
  width = Inf
)


write_csv(
  table_4_5,
  file.path(
    output_dir,
    "table_4_5_income_nb.csv"
  )
)


# ============================================================
# TABLE 4.6
# SUPPLEMENTARY IMD MODEL
# ============================================================


# ------------------------------------------------------------
# 13. IMD model results
# ------------------------------------------------------------

table_4_6 <- bind_rows(

  extract_irr(
    nb_imd,
    term = "betting_shop_count",
    scale = 1,
    label = "Betting-shop count"
  ),

  # IMD score is reported per one-unit increase
  extract_irr(
    nb_imd,
    term = "imd_score",
    scale = 1,
    label = "IMD score"
  ),

  extract_irr(
    nb_imd,
    term = "population_density",
    scale = 1000,
    label = "Population density (+1,000 residents/km2)"
  )
)


print(
  table_4_6,
  n = Inf,
  width = Inf
)


write_csv(
  table_4_6,
  file.path(
    output_dir,
    "table_4_6_imd_nb.csv"
  )
)


# ============================================================
# PART E. MODEL FIT STATISTICS
# ============================================================


# ------------------------------------------------------------
# 14. Extract fit statistics
# ------------------------------------------------------------

model_fit_statistics <- tibble(

  Model = c(
    "Income deprivation model",
    "IMD model"
  ),

  N = c(
    nobs(nb_income),
    nobs(nb_imd)
  ),

  AIC = c(
    AIC(nb_income),
    AIC(nb_imd)
  ),

  Log_likelihood = c(
    as.numeric(
      logLik(nb_income)
    ),
    as.numeric(
      logLik(nb_imd)
    )
  ),

  Theta = c(
    nb_income$theta,
    nb_imd$theta
  )
)


print(
  model_fit_statistics,
  width = Inf
)


write_csv(
  model_fit_statistics,
  file.path(
    output_dir,
    "main_model_fit_statistics.csv"
  )
)


# ============================================================
# PART F. VALIDATE KEY DISSERTATION RESULTS
# ============================================================


# ------------------------------------------------------------
# 15. Betting-shop IRR validation
# ------------------------------------------------------------

income_shop_irr <-
  exp(
    coef(nb_income)[
      "betting_shop_count"
    ]
  )

imd_shop_irr <-
  exp(
    coef(nb_imd)[
      "betting_shop_count"
    ]
  )


cat(
  "\nPrincipal Income model betting-shop IRR:",
  round(
    income_shop_irr,
    3
  ),
  "\n"
)

cat(
  "Supplementary IMD model betting-shop IRR:",
  round(
    imd_shop_irr,
    3
  ),
  "\n"
)


# Expected dissertation values are approximately:
#
# Income model:
# betting-shop IRR = 2.087
#
# IMD model:
# betting-shop IRR = 2.064

stopifnot(
  round(
    income_shop_irr,
    3
  ) == 2.087
)

stopifnot(
  round(
    imd_shop_irr,
    3
  ) == 2.064
)


# ------------------------------------------------------------
# 16. Validate Income Deprivation result
# ------------------------------------------------------------

income_irr_010 <-
  exp(
    coef(nb_income)[
      "income_score"
    ] *
      0.10
  )


cat(
  "Income deprivation IRR for +0.10:",
  round(
    income_irr_010,
    3
  ),
  "\n"
)


# Expected approximately 0.933
stopifnot(
  round(
    income_irr_010,
    3
  ) == 0.933
)


# ------------------------------------------------------------
# 17. Validate IMD result
# ------------------------------------------------------------

imd_irr <-
  exp(
    coef(nb_imd)[
      "imd_score"
    ]
  )


cat(
  "IMD IRR per one-unit increase:",
  round(
    imd_irr,
    3
  ),
  "\n"
)


# Expected approximately 1.009
stopifnot(
  round(
    imd_irr,
    3
  ) == 1.009
)


# ------------------------------------------------------------
# 18. Validate population-density result
# ------------------------------------------------------------

income_pop_density_irr <-
  exp(
    coef(nb_income)[
      "population_density"
    ] *
      1000
  )


cat(
  "Population-density IRR (+1,000/km2), Income model:",
  round(
    income_pop_density_irr,
    3
  ),
  "\n"
)


# Expected approximately 1.003


# ------------------------------------------------------------
# 19. Final completion message
# ------------------------------------------------------------

message(
  "04_main_models.R completed successfully."
)
