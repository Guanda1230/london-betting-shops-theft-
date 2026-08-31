
# ============================================================
# 07_influence_spatial_diagnostics.R
#
# MSc Dissertation:
# Betting-Shop Concentration, Deprivation and Recorded Theft
# in London: A Spatial Crime Analysis
#
# Purpose:
# Conduct case-influence and residual spatial diagnostics for
# the principal Negative Binomial models.
#
# Includes:
# - Cook's distance
# - Leverage
# - Betting-shop DFBETA
# - Westminster 013G deletion sensitivity
# - Queen-contiguity spatial weights
# - Moran's I using Pearson residuals
# - 9,999 Monte Carlo permutations
#
# Inputs:
# data/processed/final_london_lsoa_dataset_cleaned.csv
# data/processed/london_lsoa_boundary.gpkg
#
# Outputs:
# outputs/tables/table_4_12_influence_diagnostics.csv
# outputs/tables/westminster_013g_deletion_check.csv
# outputs/tables/top_influential_lsoas.csv
# outputs/tables/table_4_13_moran_diagnostics.csv
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readr)
library(dplyr)
library(sf)
library(MASS)
library(spdep)


# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

analysis_file <- file.path(
  "data",
  "processed",
  "final_london_lsoa_dataset_cleaned.csv"
)

boundary_file <- file.path(
  "data",
  "processed",
  "london_lsoa_boundary.gpkg"
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
# 3. Load analytical and spatial data
# ------------------------------------------------------------

analysis_data <- read_csv(
  analysis_file,
  show_col_types = FALSE
)

london_boundary <- st_read(
  boundary_file,
  quiet = TRUE
)


stopifnot(
  nrow(analysis_data) == 4994
)

stopifnot(
  nrow(london_boundary) == 4994
)

stopifnot(
  n_distinct(analysis_data$lsoa_code) == 4994
)

stopifnot(
  n_distinct(london_boundary$lsoa_code) == 4994
)


# ------------------------------------------------------------
# 4. Construct required variables
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    log_population = log(population),
    local_authority = as.factor(local_authority)
  )


# ============================================================
# PART A. BASELINE NEGATIVE BINOMIAL MODELS
# ============================================================


# ------------------------------------------------------------
# 5. Principal Income Deprivation model
# ------------------------------------------------------------

nb_income <- MASS::glm.nb(
  theft_count ~
    betting_shop_count +
    income_score +
    population_density +
    offset(log_population),

  data = analysis_data
)


# ------------------------------------------------------------
# 6. Supplementary IMD model
# ------------------------------------------------------------

nb_imd <- MASS::glm.nb(
  theft_count ~
    betting_shop_count +
    imd_score +
    population_density +
    offset(log_population),

  data = analysis_data
)


# ------------------------------------------------------------
# 7. Income model with local-authority fixed effects
# ------------------------------------------------------------

nb_income_la_fe <- MASS::glm.nb(
  theft_count ~
    betting_shop_count +
    income_score +
    population_density +
    local_authority +
    offset(log_population),

  data = analysis_data,

  control = glm.control(
    maxit = 100
  )
)


stopifnot(
  nb_income_la_fe$converged
)


# ============================================================
# PART B. CASE-INFLUENCE DIAGNOSTICS
# ============================================================


# ------------------------------------------------------------
# 8. Calculate influence statistics
# ------------------------------------------------------------

income_cook <- as.numeric(
  cooks.distance(nb_income)
)

imd_cook <- as.numeric(
  cooks.distance(nb_imd)
)


income_leverage <- as.numeric(
  hatvalues(nb_income)
)

imd_leverage <- as.numeric(
  hatvalues(nb_imd)
)


income_dfbeta <- as.numeric(
  dfbetas(nb_income)[
    ,
    "betting_shop_count"
  ]
)

imd_dfbeta <- as.numeric(
  dfbetas(nb_imd)[
    ,
    "betting_shop_count"
  ]
)


# ------------------------------------------------------------
# 9. Diagnostic screening thresholds
# ------------------------------------------------------------
#
# Dissertation thresholds:
#
# Cook's distance: 4 / n
# Leverage:        2p / n
# DFBETA:          2 / sqrt(n)
#
# n = 4,994
# p = 4 estimated regression coefficients:
# intercept, betting-shop count, deprivation,
# population density.
#
# The population offset is not an estimated coefficient.

n <- nrow(analysis_data)

p <- 4


cook_threshold <-
  4 / n

leverage_threshold <-
  2 * p / n

dfbeta_threshold <-
  2 / sqrt(n)


cat(
  "\nCook's distance threshold:",
  cook_threshold,
  "\n"
)

cat(
  "Leverage threshold:",
  leverage_threshold,
  "\n"
)

cat(
  "DFBETA threshold:",
  dfbeta_threshold,
  "\n"
)


# ------------------------------------------------------------
# 10. Table 4.12 influence summary
# ------------------------------------------------------------

table_4_12 <- tibble(

  Model = c(
    "Income deprivation model",
    "IMD model"
  ),

  Maximum_Cooks_distance = c(
    max(
      income_cook,
      na.rm = TRUE
    ),
    max(
      imd_cook,
      na.rm = TRUE
    )
  ),

  Cooks_threshold = cook_threshold,

  Above_Cooks_threshold = c(
    sum(
      income_cook >
        cook_threshold,
      na.rm = TRUE
    ),
    sum(
      imd_cook >
        cook_threshold,
      na.rm = TRUE
    )
  ),

  Maximum_leverage = c(
    max(
      income_leverage,
      na.rm = TRUE
    ),
    max(
      imd_leverage,
      na.rm = TRUE
    )
  ),

  Leverage_threshold =
    leverage_threshold,

  Above_leverage_threshold = c(
    sum(
      income_leverage >
        leverage_threshold,
      na.rm = TRUE
    ),
    sum(
      imd_leverage >
        leverage_threshold,
      na.rm = TRUE
    )
  ),

  Maximum_absolute_betting_DFBETA = c(
    max(
      abs(income_dfbeta),
      na.rm = TRUE
    ),
    max(
      abs(imd_dfbeta),
      na.rm = TRUE
    )
  ),

  DFBETA_threshold =
    dfbeta_threshold,

  Above_DFBETA_threshold = c(
    sum(
      abs(income_dfbeta) >
        dfbeta_threshold,
      na.rm = TRUE
    ),
    sum(
      abs(imd_dfbeta) >
        dfbeta_threshold,
      na.rm = TRUE
    )
  )
)


print(
  table_4_12,
  width = Inf
)


write_csv(
  table_4_12,
  file.path(
    output_dir,
    "table_4_12_influence_diagnostics.csv"
  )
)


# ------------------------------------------------------------
# 11. Validate published influence diagnostics
# ------------------------------------------------------------

# Expected dissertation results:
#
# Income:
# Cook max       ≈ 1.83; above threshold = 248
# Leverage max   ≈ 0.0841; above threshold = 377
# |DFBETA| max   ≈ 0.830; above threshold = 184
#
# IMD:
# Cook max       ≈ 1.64; above threshold = 241
# Leverage max   ≈ 0.0848; above threshold = 367
# |DFBETA| max   ≈ 0.807; above threshold = 188

stopifnot(
  table_4_12$Above_Cooks_threshold[1] == 248,
  table_4_12$Above_Cooks_threshold[2] == 241,

  table_4_12$Above_leverage_threshold[1] == 377,
  table_4_12$Above_leverage_threshold[2] == 367,

  table_4_12$Above_DFBETA_threshold[1] == 184,
  table_4_12$Above_DFBETA_threshold[2] == 188
)


# ============================================================
# PART C. IDENTIFY INFLUENTIAL LSOAs
# ============================================================


# ------------------------------------------------------------
# 12. Attach diagnostics to analytical dataset
# ------------------------------------------------------------

influence_data <- analysis_data %>%

  mutate(

    income_cooks_distance =
      income_cook,

    imd_cooks_distance =
      imd_cook,

    income_leverage =
      income_leverage,

    imd_leverage =
      imd_leverage,

    income_betting_dfbeta =
      income_dfbeta,

    imd_betting_dfbeta =
      imd_dfbeta
  )


# ------------------------------------------------------------
# 13. Top five observations by Cook's distance
# ------------------------------------------------------------

top_income_influence <- influence_data %>%

  arrange(
    desc(
      income_cooks_distance
    )
  ) %>%

  slice_head(
    n = 5
  ) %>%

  transmute(

    Model =
      "Income deprivation model",

    lsoa_code,
    lsoa_name,
    local_authority,
    theft_count,
    theft_rate_1000,
    population,
    betting_shop_count,

    Cooks_distance =
      income_cooks_distance,

    Leverage =
      income_leverage,

    Betting_shop_DFBETA =
      income_betting_dfbeta
  )


top_imd_influence <- influence_data %>%

  arrange(
    desc(
      imd_cooks_distance
    )
  ) %>%

  slice_head(
    n = 5
  ) %>%

  transmute(

    Model =
      "IMD model",

    lsoa_code,
    lsoa_name,
    local_authority,
    theft_count,
    theft_rate_1000,
    population,
    betting_shop_count,

    Cooks_distance =
      imd_cooks_distance,

    Leverage =
      imd_leverage,

    Betting_shop_DFBETA =
      imd_betting_dfbeta
  )


top_influential_lsoas <- bind_rows(
  top_income_influence,
  top_imd_influence
)


print(
  top_influential_lsoas,
  n = Inf,
  width = Inf
)


write_csv(
  top_influential_lsoas,
  file.path(
    output_dir,
    "top_influential_lsoas.csv"
  )
)


# ============================================================
# PART D. WESTMINSTER 013G DELETION CHECK
# ============================================================


# ------------------------------------------------------------
# 14. Identify Westminster 013G
# ------------------------------------------------------------

westminster_013g_code <-
  "E01035716"


westminster_013g <- influence_data %>%

  filter(
    lsoa_code ==
      westminster_013g_code
  )


stopifnot(
  nrow(
    westminster_013g
  ) == 1
)


print(
  westminster_013g %>%
    select(
      lsoa_code,
      lsoa_name,
      local_authority,
      theft_count,
      theft_rate_1000,
      population,
      betting_shop_count,
      income_cooks_distance,
      imd_cooks_distance,
      income_leverage,
      imd_leverage,
      income_betting_dfbeta,
      imd_betting_dfbeta
    ),
  width = Inf
)


# ------------------------------------------------------------
# 15. Re-estimate models without Westminster 013G
# ------------------------------------------------------------

analysis_without_westminster <- analysis_data %>%

  filter(
    lsoa_code !=
      westminster_013g_code
  )


nb_income_without_westminster <- MASS::glm.nb(
  theft_count ~
    betting_shop_count +
    income_score +
    population_density +
    offset(log_population),

  data =
    analysis_without_westminster
)


nb_imd_without_westminster <- MASS::glm.nb(
  theft_count ~
    betting_shop_count +
    imd_score +
    population_density +
    offset(log_population),

  data =
    analysis_without_westminster
)


# ------------------------------------------------------------
# 16. Compare betting-shop IRRs
# ------------------------------------------------------------

westminster_deletion_check <- tibble(

  Model = c(
    "Income deprivation model",
    "IMD model"
  ),

  Full_sample_IRR = c(

    exp(
      coef(nb_income)[
        "betting_shop_count"
      ]
    ),

    exp(
      coef(nb_imd)[
        "betting_shop_count"
      ]
    )
  ),

  IRR_without_Westminster_013G = c(

    exp(
      coef(
        nb_income_without_westminster
      )[
        "betting_shop_count"
      ]
    ),

    exp(
      coef(
        nb_imd_without_westminster
      )[
        "betting_shop_count"
      ]
    )
  )
) %>%

  mutate(

    Percentage_change =
      100 *
      (
        IRR_without_Westminster_013G -
          Full_sample_IRR
      ) /
      Full_sample_IRR
  )


print(
  westminster_deletion_check,
  width = Inf
)


write_csv(
  westminster_deletion_check,
  file.path(
    output_dir,
    "westminster_013g_deletion_check.csv"
  )
)


# Expected approximately:
#
# Income:
# 2.09 -> 1.99
#
# IMD:
# 2.06 -> 1.97

cat(
  "\nIncome IRR without Westminster 013G:",
  round(
    westminster_deletion_check$
      IRR_without_Westminster_013G[1],
    2
  ),
  "\n"
)

cat(
  "IMD IRR without Westminster 013G:",
  round(
    westminster_deletion_check$
      IRR_without_Westminster_013G[2],
    2
  ),
  "\n"
)


stopifnot(
  round(
    westminster_deletion_check$
      IRR_without_Westminster_013G[1],
    2
  ) == 1.99,

  round(
    westminster_deletion_check$
      IRR_without_Westminster_013G[2],
    2
  ) == 1.97
)


# ============================================================
# PART E. PREPARE SPATIAL DATA
# ============================================================


# ------------------------------------------------------------
# 17. Join analytical variables to geometry by LSOA code
# ------------------------------------------------------------
#
# Explicit joining by LSOA code avoids relying on the row order
# of the CSV and spatial files.

spatial_data <- london_boundary %>%

  select(
    lsoa_code,
    geometry
  ) %>%

  left_join(
    analysis_data %>%
      select(
        lsoa_code,
        local_authority
      ),
    by = "lsoa_code"
  )


stopifnot(
  nrow(spatial_data) == 4994
)

stopifnot(
  !anyDuplicated(
    spatial_data$lsoa_code
  )
)


# ============================================================
# PART F. QUEEN-CONTIGUITY SPATIAL WEIGHTS
# ============================================================


# ------------------------------------------------------------
# 18. Construct Queen neighbours
# ------------------------------------------------------------

queen_nb <- poly2nb(
  spatial_data,
  queen = TRUE
)


# ------------------------------------------------------------
# 19. Neighbour diagnostics
# ------------------------------------------------------------

neighbour_counts <- card(
  queen_nb
)


cat(
  "\nNumber of LSOAs:",
  length(queen_nb),
  "\n"
)

cat(
  "Zero-neighbour LSOAs:",
  sum(
    neighbour_counts == 0
  ),
  "\n"
)

cat(
  "Mean number of neighbours:",
  mean(
    neighbour_counts
  ),
  "\n"
)


# ------------------------------------------------------------
# 20. Row-standardised spatial weights
# ------------------------------------------------------------

queen_listw <- nb2listw(
  queen_nb,
  style = "W",
  zero.policy = TRUE
)


# ============================================================
# PART G. ALIGN MODEL RESIDUALS WITH SPATIAL DATA
# ============================================================


# ------------------------------------------------------------
# 21. Extract Pearson residuals
# ------------------------------------------------------------

income_residual_data <- tibble(

  lsoa_code =
    analysis_data$lsoa_code,

  income_residual =
    residuals(
      nb_income,
      type = "pearson"
    )
)


imd_residual_data <- tibble(

  lsoa_code =
    analysis_data$lsoa_code,

  imd_residual =
    residuals(
      nb_imd,
      type = "pearson"
    )
)


la_fe_residual_data <- tibble(

  lsoa_code =
    analysis_data$lsoa_code,

  la_fe_residual =
    residuals(
      nb_income_la_fe,
      type = "pearson"
    )
)


# ------------------------------------------------------------
# 22. Join residuals to spatial ordering
# ------------------------------------------------------------

spatial_residuals <- spatial_data %>%

  left_join(
    income_residual_data,
    by = "lsoa_code"
  ) %>%

  left_join(
    imd_residual_data,
    by = "lsoa_code"
  ) %>%

  left_join(
    la_fe_residual_data,
    by = "lsoa_code"
  )


stopifnot(
  !anyNA(
    spatial_residuals$income_residual
  )
)

stopifnot(
  !anyNA(
    spatial_residuals$imd_residual
  )
)

stopifnot(
  !anyNA(
    spatial_residuals$la_fe_residual
  )
)


# ============================================================
# PART H. MORAN'S I
# ============================================================


# ------------------------------------------------------------
# 23. Reproducible Monte Carlo tests
# ------------------------------------------------------------

set.seed(2026)


moran_income <- moran.mc(
  spatial_residuals$income_residual,
  listw = queen_listw,
  nsim = 9999,
  zero.policy = TRUE
)


set.seed(2026)


moran_imd <- moran.mc(
  spatial_residuals$imd_residual,
  listw = queen_listw,
  nsim = 9999,
  zero.policy = TRUE
)


set.seed(2026)


moran_la_fe <- moran.mc(
  spatial_residuals$la_fe_residual,
  listw = queen_listw,
  nsim = 9999,
  zero.policy = TRUE
)


# ------------------------------------------------------------
# 24. Display Moran tests
# ------------------------------------------------------------

print(
  moran_income
)

print(
  moran_imd
)

print(
  moran_la_fe
)


# ------------------------------------------------------------
# 25. Extract observed Moran's I
# ------------------------------------------------------------

income_moran_i <-
  unname(
    moran_income$statistic
  )

imd_moran_i <-
  unname(
    moran_imd$statistic
  )

la_fe_moran_i <-
  unname(
    moran_la_fe$statistic
  )


# ============================================================
# TABLE 4.13
# ============================================================


# ------------------------------------------------------------
# 26. Residual spatial autocorrelation table
# ------------------------------------------------------------

table_4_13 <- tibble(

  Specification = c(
    "Principal Income Deprivation model",
    "Supplementary IMD model",
    "Local-authority fixed-effects Income model"
  ),

  Morans_I = c(
    income_moran_i,
    imd_moran_i,
    la_fe_moran_i
  ),

  Permutations =
    9999,

  Monte_Carlo_p_value = c(
    moran_income$p.value,
    moran_imd$p.value,
    moran_la_fe$p.value
  )
)


print(
  table_4_13,
  width = Inf
)


write_csv(
  table_4_13,
  file.path(
    output_dir,
    "table_4_13_moran_diagnostics.csv"
  )
)


# ------------------------------------------------------------
# 27. Validate dissertation Moran's I values
# ------------------------------------------------------------
#
# Expected published results:
#
# Principal Income model       I = 0.398
# Supplementary IMD model      I = 0.416
# Local-authority FE model     I = 0.137
#
# All Monte Carlo p-values < .001.

cat(
  "\nIncome Moran's I:",
  round(
    income_moran_i,
    3
  ),
  "\n"
)

cat(
  "IMD Moran's I:",
  round(
    imd_moran_i,
    3
  ),
  "\n"
)

cat(
  "LA fixed-effects Moran's I:",
  round(
    la_fe_moran_i,
    3
  ),
  "\n"
)


stopifnot(
  round(
    income_moran_i,
    3
  ) == 0.398,

  round(
    imd_moran_i,
    3
  ) == 0.416,

  round(
    la_fe_moran_i,
    3
  ) == 0.137
)


stopifnot(
  moran_income$p.value < 0.001,
  moran_imd$p.value < 0.001,
  moran_la_fe$p.value < 0.001
)


# ------------------------------------------------------------
# 28. Completion
# ------------------------------------------------------------

message(
  "07_influence_spatial_diagnostics.R completed successfully."
)
