
# ============================================================
# 02_data_preparation.R
#
# MSc Dissertation:
# Betting-Shop Concentration, Deprivation and Recorded Theft
# in London: A Spatial Crime Analysis
#
# Purpose:
# Prepare betting-shop, deprivation, population and spatial
# data and construct the final London LSOA analytical dataset.
#
# Requires:
# scripts/01_crime_cleaning_2024.R to have been run first.
#
# Main outputs:
# data/processed/betting_lsoa.csv
# data/processed/deprivation_lsoa.csv
# data/processed/population_lsoa_2024.csv
# data/processed/london_lsoa_boundary.gpkg
# data/processed/final_london_lsoa_dataset_cleaned.csv
# data/processed/final_london_lsoa_dataset.gpkg
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(sf)


# ------------------------------------------------------------
# 2. Project paths
# ------------------------------------------------------------

raw_dir <- file.path(
  "data",
  "raw"
)

processed_dir <- file.path(
  "data",
  "processed"
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. Greater London local authorities
# ------------------------------------------------------------

london_boroughs <- c(
  "Barking and Dagenham",
  "Barnet",
  "Bexley",
  "Brent",
  "Bromley",
  "Camden",
  "Croydon",
  "Ealing",
  "Enfield",
  "Greenwich",
  "Hackney",
  "Hammersmith and Fulham",
  "Haringey",
  "Harrow",
  "Havering",
  "Hillingdon",
  "Hounslow",
  "Islington",
  "Kensington and Chelsea",
  "Kingston upon Thames",
  "Lambeth",
  "Lewisham",
  "Merton",
  "Newham",
  "Redbridge",
  "Richmond upon Thames",
  "Southwark",
  "Sutton",
  "Tower Hamlets",
  "Waltham Forest",
  "Wandsworth",
  "Westminster",
  "City of London"
)


# ============================================================
# PART A. POSTCODE / LSOA LOOKUP
# ============================================================


# ------------------------------------------------------------
# 4. Locate postcode-to-LSOA lookup
# ------------------------------------------------------------

lookup_files <- list.files(
  raw_dir,
  pattern = "PCD.*LSOA.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(lookup_files) == 0) {
  stop(
    "Postcode-to-LSOA lookup file not found in data/raw/."
  )
}

lookup_file <- lookup_files[1]

message(
  "Using postcode lookup: ",
  basename(lookup_file)
)


postcode_lookup_raw <- read_csv(
  lookup_file,
  show_col_types = FALSE
) %>%
  clean_names()


# ------------------------------------------------------------
# 5. Clean postcode lookup
# ------------------------------------------------------------

clean_postcode <- function(x) {

  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_replace_all(
      "[^A-Z0-9]",
      ""
    )
}


postcode_lookup_clean <- postcode_lookup_raw %>%
  transmute(
    postcode_clean =
      clean_postcode(pcds),

    lsoa_code =
      lsoa21cd,

    lad_code =
      ladcd,

    local_authority =
      ladnm
  ) %>%

  filter(
    !is.na(postcode_clean),
    postcode_clean != ""
  ) %>%

  distinct(
    postcode_clean,
    .keep_all = TRUE
  )


# London LSOA lookup
london_lsoa_lookup <- postcode_lookup_clean %>%
  filter(
    local_authority %in%
      london_boroughs
  ) %>%

  distinct(
    lsoa_code,
    local_authority
  )


# Final study area should contain 4,994 LSOAs
stopifnot(
  n_distinct(
    london_lsoa_lookup$lsoa_code
  ) == 4994
)


# ============================================================
# PART B. BETTING-SHOP DATA
# ============================================================


# ------------------------------------------------------------
# 6. Locate Gambling Commission premises register
# ------------------------------------------------------------

premises_files <- list.files(
  raw_dir,
  pattern = "premises.*licence.*register.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(premises_files) == 0) {
  stop(
    paste(
      "Gambling Commission premises register",
      "not found in data/raw/."
    )
  )
}

premises_file <- premises_files[1]

message(
  "Using premises register: ",
  basename(premises_file)
)


premises_raw <- read_excel(
  premises_file
) %>%
  clean_names()


# ------------------------------------------------------------
# 7. Select Betting Shop premises
# ------------------------------------------------------------
#
# The final dissertation uses premises explicitly classified
# as "Betting Shop". Other gambling-premises categories are
# not included.

betting_raw <- premises_raw %>%

  mutate(
    premises_activity =
      str_squish(
        as.character(
          premises_activity
        )
      )
  ) %>%

  filter(
    premises_activity ==
      "Betting Shop"
  )


cat(
  "\nRows classified as Betting Shop:",
  nrow(betting_raw),
  "\n"
)


# ------------------------------------------------------------
# 8. Standardise addresses and identify physical premises
# ------------------------------------------------------------

clean_address_part <- function(x) {

  coalesce(
    as.character(x),
    ""
  ) %>%

    str_to_upper() %>%

    str_replace_all(
      "[^A-Z0-9]+",
      " "
    ) %>%

    str_squish()
}


betting_clean <- betting_raw %>%

  mutate(

    address_1_clean =
      clean_address_part(
        address_line_1
      ),

    address_2_clean =
      clean_address_part(
        address_line_2
      ),

    city_clean =
      clean_address_part(
        city
      ),

    postcode_clean =
      clean_postcode(
        postcode
      ),

    valid_location =
      postcode_clean != "" &
      (
        address_1_clean != "" |
          address_2_clean != ""
      ),

    physical_shop_key =
      paste(
        address_1_clean,
        address_2_clean,
        city_clean,
        postcode_clean,
        sep = "|"
      )
  )


# ------------------------------------------------------------
# 9. Remove duplicate physical premises
# ------------------------------------------------------------

location_duplicates <- betting_clean %>%

  filter(
    valid_location
  ) %>%

  count(
    physical_shop_key,
    name = "rows"
  ) %>%

  filter(
    rows > 1
  )


duplicate_summary <- tibble(

  betting_shop_rows =
    nrow(betting_clean),

  incomplete_locations =
    sum(
      !betting_clean$valid_location
    ),

  duplicated_location_groups =
    nrow(location_duplicates),

  extra_rows_at_same_location =
    sum(
      location_duplicates$rows - 1
    )
)

print(
  duplicate_summary,
  width = Inf
)


# Retain one record for each physical premises
betting_distinct <- betting_clean %>%

  filter(
    valid_location
  ) %>%

  arrange(
    physical_shop_key,
    account_number
  ) %>%

  distinct(
    physical_shop_key,
    .keep_all = TRUE
  )


# ------------------------------------------------------------
# 10. Correct two identified postcode records
# ------------------------------------------------------------
#
# Two premises required postcode corrections identified during
# manual data checking.

betting_distinct <- betting_distinct %>%

  mutate(

    postcode = case_when(

      str_detect(
        str_to_upper(
          coalesce(
            address_line_2,
            ""
          )
        ),
        "115 RYE ROAD"
      ) ~ "EN11 0JP",

      str_detect(
        str_to_upper(
          coalesce(
            address_line_2,
            ""
          )
        ),
        "32-33 GRAND PARADE"
      ) ~ "N4 1LG",

      TRUE ~ postcode
    ),

    postcode_clean =
      clean_postcode(
        postcode
      )
  )


# ------------------------------------------------------------
# 11. Assign betting shops to LSOAs
# ------------------------------------------------------------

betting_matched <- betting_distinct %>%

  select(
    -any_of(
      c(
        "lsoa_code",
        "lad_code",
        "local_authority"
      )
    )
  ) %>%

  left_join(
    postcode_lookup_clean,
    by = "postcode_clean"
  )


betting_match_check <- betting_matched %>%

  summarise(

    distinct_physical_shops =
      n(),

    matched_to_lsoa =
      sum(
        !is.na(lsoa_code)
      ),

    unmatched =
      sum(
        is.na(lsoa_code)
      )
  )

print(
  betting_match_check,
  width = Inf
)


# ------------------------------------------------------------
# 12. Restrict betting shops to Greater London
# ------------------------------------------------------------

betting_london <- betting_matched %>%

  filter(
    !is.na(lsoa_code),
    local_authority %in%
      london_boroughs
  )


betting_lsoa <- betting_london %>%

  count(
    lsoa_code,
    name = "betting_shop_count"
  )


# Dissertation totals:
# 1,256 betting shops
# 842 LSOAs containing at least one betting shop

stopifnot(
  sum(
    betting_lsoa$betting_shop_count
  ) == 1256
)

stopifnot(
  nrow(betting_lsoa) == 842
)


cat(
  "\nLondon betting shops:",
  sum(betting_lsoa$betting_shop_count),
  "\n"
)

cat(
  "LSOAs with betting shops:",
  nrow(betting_lsoa),
  "\n"
)


# Save processed betting-shop data
write_csv(
  betting_lsoa,
  file.path(
    processed_dir,
    "betting_lsoa.csv"
  )
)


# ============================================================
# PART C. DEPRIVATION DATA
# ============================================================


# ------------------------------------------------------------
# 13. Locate English Indices of Deprivation 2025 File 7
# ------------------------------------------------------------

iod_files <- list.files(
  raw_dir,
  pattern =
    "File_7.*IoD2025.*\\.csv$|IoD2025.*All.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(iod_files) == 0) {
  stop(
    "IoD 2025 File 7 not found in data/raw/."
  )
}

iod_file <- iod_files[1]

message(
  "Using deprivation file: ",
  basename(iod_file)
)


iod_raw <- read_csv(
  iod_file,
  show_col_types = FALSE
) %>%
  clean_names()


# ------------------------------------------------------------
# 14. Select deprivation variables
# ------------------------------------------------------------

deprivation_lsoa <- iod_raw %>%

  transmute(

    lsoa_code =
      lsoa_code_2021,

    lsoa_name =
      lsoa_name_2021,

    imd_score =
      index_of_multiple_deprivation_imd_score,

    imd_rank =
      index_of_multiple_deprivation_imd_rank_where_1_is_most_deprived,

    imd_decile =
      index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as,

    income_score =
      income_score_rate,

    income_rank =
      income_rank_where_1_is_most_deprived,

    income_decile =
      income_decile_where_1_is_most_deprived_10_percent_of_lso_as
  )


# Check uniqueness
stopifnot(
  nrow(deprivation_lsoa) ==
    n_distinct(
      deprivation_lsoa$lsoa_code
    )
)


write_csv(
  deprivation_lsoa,
  file.path(
    processed_dir,
    "deprivation_lsoa.csv"
  )
)


# ============================================================
# PART D. POPULATION DATA
# ============================================================


# ------------------------------------------------------------
# 15. Locate ONS Mid-2024 LSOA population workbook
# ------------------------------------------------------------

population_files <- list.files(
  raw_dir,
  pattern =
    "sapelsoa.*2022.*2024.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(population_files) == 0) {
  stop(
    paste(
      "ONS Mid-2024 LSOA population workbook",
      "not found in data/raw/."
    )
  )
}

population_file <- population_files[1]

message(
  "Using population file: ",
  basename(population_file)
)


# ------------------------------------------------------------
# 16. Identify header row in Mid-2024 worksheet
# ------------------------------------------------------------

population_preview <- read_excel(
  population_file,
  sheet = "Mid-2024 LSOA 2021",
  col_names = FALSE
)


header_row <- which(

  apply(
    population_preview,
    1,
    function(row) {

      row_text <-
        as.character(row)

      any(
        str_detect(
          row_text,
          regex(
            "LSOA",
            ignore_case = TRUE
          )
        ),
        na.rm = TRUE
      ) &&

        any(
          str_detect(
            row_text,
            regex(
              "All ages|Total",
              ignore_case = TRUE
            )
          ),
          na.rm = TRUE
        )
    }
  )
)[1]


if (is.na(header_row)) {
  stop(
    "Could not identify population table header row."
  )
}


population_raw <- read_excel(
  population_file,
  sheet = "Mid-2024 LSOA 2021",
  skip = header_row - 1
) %>%
  clean_names()


# ------------------------------------------------------------
# 17. Create LSOA population dataset
# ------------------------------------------------------------

population_lsoa_2024 <- population_raw %>%

  transmute(

    lsoa_code =
      lsoa_2021_code,

    lsoa_name =
      lsoa_2021_name,

    population =
      as.numeric(total)
  ) %>%

  filter(
    !is.na(lsoa_code),
    !is.na(population)
  )


write_csv(
  population_lsoa_2024,
  file.path(
    processed_dir,
    "population_lsoa_2024.csv"
  )
)


# ============================================================
# PART E. LSOA BOUNDARY
# ============================================================


# ------------------------------------------------------------
# 18. Locate LSOA 2021 boundary file
# ------------------------------------------------------------

boundary_files <- list.files(
  raw_dir,
  pattern = "\\.(shp|gpkg)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

boundary_files <- boundary_files[
  str_detect(
    boundary_files,
    regex(
      "LSOA|Lower.*layer.*Super.*Output",
      ignore_case = TRUE
    )
  )
]


if (length(boundary_files) == 0) {
  stop(
    "LSOA boundary file not found in data/raw/."
  )
}

boundary_file <- boundary_files[1]

message(
  "Using boundary file: ",
  basename(boundary_file)
)


lsoa_boundary <- st_read(
  boundary_file,
  quiet = TRUE
) %>%
  clean_names()


# ------------------------------------------------------------
# 19. Standardise LSOA boundary fields
# ------------------------------------------------------------

lsoa_boundary <- lsoa_boundary %>%

  rename(
    lsoa_code =
      lsoa21cd,

    lsoa_name =
      lsoa21nm
  )


# Restrict boundaries to Greater London
london_lsoa_boundary <- lsoa_boundary %>%

  inner_join(
    london_lsoa_lookup,
    by = "lsoa_code"
  )


stopifnot(
  nrow(
    london_lsoa_boundary
  ) == 4994
)


# ------------------------------------------------------------
# 20. Calculate LSOA area
# ------------------------------------------------------------
#
# British National Grid is used for area calculations.

london_lsoa_boundary <- london_lsoa_boundary %>%

  st_transform(
    27700
  ) %>%

  mutate(
    area_km2 =
      as.numeric(
        st_area(geometry)
      ) / 1000000
  )


stopifnot(
  all(
    london_lsoa_boundary$area_km2 > 0
  )
)


st_write(
  london_lsoa_boundary,
  file.path(
    processed_dir,
    "london_lsoa_boundary.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)


# ============================================================
# PART F. CONSTRUCT FINAL ANALYTICAL DATASET
# ============================================================


# ------------------------------------------------------------
# 21. Read cleaned 2024 crime data from Script 01
# ------------------------------------------------------------

crime_file <- file.path(
  processed_dir,
  "crime_lsoa_2024_cleaned.csv"
)

if (!file.exists(crime_file)) {
  stop(
    paste(
      "crime_lsoa_2024_cleaned.csv not found.",
      "Run scripts/01_crime_cleaning_2024.R first."
    )
  )
}


crime_lsoa_2024 <- read_csv(
  crime_file,
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 22. Merge all LSOA-level datasets
# ------------------------------------------------------------

final_london <- london_lsoa_boundary %>%

  left_join(
    crime_lsoa_2024,
    by = "lsoa_code"
  ) %>%

  left_join(
    betting_lsoa,
    by = "lsoa_code"
  ) %>%

  left_join(
    deprivation_lsoa %>%
      select(
        -lsoa_name
      ),
    by = "lsoa_code"
  ) %>%

  left_join(
    population_lsoa_2024 %>%
      select(
        lsoa_code,
        population
      ),
    by = "lsoa_code"
  )


# ------------------------------------------------------------
# 23. Replace structural zeros
# ------------------------------------------------------------

final_london <- final_london %>%

  mutate(

    across(
      c(
        theft_count,
        shoplifting_count,
        theft_from_person_count,
        bicycle_theft_count,
        other_theft_count,
        betting_shop_count
      ),
      ~ replace_na(
        .x,
        0
      )
    )
  )


# ------------------------------------------------------------
# 24. Construct analytical variables
# ------------------------------------------------------------

final_london <- final_london %>%

  mutate(

    theft_rate_1000 =
      theft_count /
      population *
      1000,

    shoplifting_rate_1000 =
      shoplifting_count /
      population *
      1000,

    theft_from_person_rate_1000 =
      theft_from_person_count /
      population *
      1000,

    betting_shop_presence =
      as.integer(
        betting_shop_count > 0
      ),

    betting_shops_1000 =
      betting_shop_count /
      population *
      1000,

    betting_shop_density_km2 =
      betting_shop_count /
      area_km2,

    population_density =
      population /
      area_km2,

    log_population =
      log(population)
  )


# ------------------------------------------------------------
# 25. Validate final analytical dataset
# ------------------------------------------------------------

final_check <- final_london %>%

  st_drop_geometry() %>%

  summarise(

    number_of_lsoas =
      n(),

    total_theft =
      sum(
        theft_count
      ),

    total_shoplifting =
      sum(
        shoplifting_count
      ),

    total_theft_from_person =
      sum(
        theft_from_person_count
      ),

    total_bicycle_theft =
      sum(
        bicycle_theft_count
      ),

    total_other_theft =
      sum(
        other_theft_count
      ),

    total_betting_shops =
      sum(
        betting_shop_count
      ),

    lsoas_with_betting_shops =
      sum(
        betting_shop_count > 0
      ),

    missing_population =
      sum(
        is.na(population)
      ),

    missing_imd =
      sum(
        is.na(imd_score)
      ),

    missing_income =
      sum(
        is.na(income_score)
      ),

    missing_area =
      sum(
        is.na(area_km2)
      )
  )


print(
  final_check,
  width = Inf
)


# Expected dissertation totals
stopifnot(

  final_check$number_of_lsoas ==
    4994,

  final_check$total_theft ==
    310108,

  final_check$total_shoplifting ==
    84123,

  final_check$total_theft_from_person ==
    98547,

  final_check$total_bicycle_theft ==
    15532,

  final_check$total_other_theft ==
    111906,

  final_check$total_betting_shops ==
    1256,

  final_check$lsoas_with_betting_shops ==
    842,

  final_check$missing_population ==
    0,

  final_check$missing_imd ==
    0,

  final_check$missing_income ==
    0,

  final_check$missing_area ==
    0
)


# ------------------------------------------------------------
# 26. Save final analytical datasets
# ------------------------------------------------------------

# Spatial version
st_write(
  final_london,
  file.path(
    processed_dir,
    "final_london_lsoa_dataset.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)


# Non-spatial CSV used in regression analyses
final_london_csv <- final_london %>%
  st_drop_geometry()


write_csv(
  final_london_csv,
  file.path(
    processed_dir,
    "final_london_lsoa_dataset_cleaned.csv"
  )
)


message(
  "02_data_preparation.R completed successfully."
)
