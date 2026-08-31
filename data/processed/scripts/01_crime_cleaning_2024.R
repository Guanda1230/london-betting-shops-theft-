
# ============================================================
# 01_crime_cleaning_2024.R
#
# MSc Dissertation:
# Betting-Shop Concentration, Deprivation and Recorded Theft
# in London: A Spatial Crime Analysis
#
# Purpose:
# Clean and aggregate Police.uk street-level theft data for
# Metropolitan Police and City of London Police, Jan-Dec 2024.
#
# Output:
# data/processed/crime_lsoa_2024_cleaned.csv
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(janitor)


# ------------------------------------------------------------
# 2. Project paths
# ------------------------------------------------------------

crime_dir <- file.path(
  "data",
  "raw",
  "crime"
)

lookup_file <- file.path(
  "data",
  "raw",
  "lookups",
  "PCD_OA21_LSOA21_MSOA21_LAD_MAY24_UK_LU.csv"
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
# 3. Identify 2024 Police.uk street files
# ------------------------------------------------------------

all_street_files <- list.files(
  crime_dir,
  pattern = "-street\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

crime_files <- all_street_files[
  grepl(
    paste0(
      "^2024-(0[1-9]|1[0-2])-",
      "(metropolitan|city-of-london)-street\\.csv$"
    ),
    basename(all_street_files),
    ignore.case = TRUE
  )
]

# Dissertation uses 12 Metropolitan Police files
# and 12 City of London Police files.
stopifnot(length(crime_files) == 24)

print(sort(basename(crime_files)))


# ------------------------------------------------------------
# 4. Import and combine crime records
# ------------------------------------------------------------

crime_raw <- map_dfr(
  crime_files,
  function(file_path) {

    read_csv(
      file_path,
      col_types = cols(.default = col_character()),
      progress = FALSE
    ) %>%

      transmute(
        crime_id = `Crime ID`,
        month = Month,
        reported_by = `Reported by`,
        crime_type = `Crime type`,
        lsoa_code = `LSOA code`,
        lsoa_name = `LSOA name`,
        longitude = Longitude,
        latitude = Latitude
      )
  }
)


# Check imported data
raw_data_check <- crime_raw %>%
  summarise(
    raw_records = n(),
    months = n_distinct(month),
    police_forces = n_distinct(reported_by)
  )

print(raw_data_check)

crime_raw %>%
  count(reported_by, name = "raw_records") %>%
  print()


# Expected raw total reported in dissertation:
stopifnot(nrow(crime_raw) == 1149321)


# ------------------------------------------------------------
# 5. Select theft categories used in the dissertation
# ------------------------------------------------------------

theft_types <- c(
  "Shoplifting",
  "Theft from the person",
  "Bicycle theft",
  "Other theft"
)

crime_candidate <- crime_raw %>%
  filter(crime_type %in% theft_types)

cat(
  "\nCandidate theft records:",
  nrow(crime_candidate),
  "\n"
)

# Expected candidate total:
stopifnot(nrow(crime_candidate) == 311506)


# ------------------------------------------------------------
# 6. Check exact duplicate rows
# ------------------------------------------------------------
#
# Exact duplicate rows are reported diagnostically.
# They are NOT removed separately because duplicate Crime IDs
# are handled below.

exact_duplicate_rows <- sum(
  duplicated(crime_candidate)
)

cat(
  "Exact duplicate rows:",
  exact_duplicate_rows,
  "\n"
)


# ------------------------------------------------------------
# 7. Audit duplicate Crime IDs
# ------------------------------------------------------------

duplicate_id_audit <- crime_candidate %>%
  filter(
    !is.na(crime_id),
    crime_id != ""
  ) %>%

  group_by(crime_id) %>%

  filter(n() > 1) %>%

  summarise(
    records = n(),

    distinct_core_records = n_distinct(
      paste(
        month,
        reported_by,
        crime_type,
        ifelse(
          is.na(lsoa_code) | lsoa_code == "",
          "<NA>",
          lsoa_code
        ),
        sep = "|"
      )
    ),

    .groups = "drop"
  )


# Duplicate IDs with identical core information:
# retain one record.
consistent_duplicate_ids <- duplicate_id_audit %>%
  filter(distinct_core_records == 1) %>%
  pull(crime_id)


# Duplicate IDs with conflicting core information:
# exclude the entire Crime ID group.
conflicting_ids <- duplicate_id_audit %>%
  filter(distinct_core_records > 1) %>%
  pull(crime_id)


# ------------------------------------------------------------
# 8. Clean duplicate Crime IDs
# ------------------------------------------------------------

crime_after_duplicate_cleaning <- bind_rows(

  # Records without Crime ID cannot be deduplicated by ID,
  # so they are retained at this stage.
  crime_candidate %>%
    filter(
      is.na(crime_id) |
        crime_id == ""
    ),

  # For valid Crime IDs:
  # remove conflicting groups and retain one observation
  # for otherwise duplicated IDs.
  crime_candidate %>%
    filter(
      !is.na(crime_id),
      crime_id != "",
      !crime_id %in% conflicting_ids
    ) %>%
    distinct(
      crime_id,
      .keep_all = TRUE
    )
)


# Cleaning audit
duplicate_cleaning_check <- tibble(
  candidate_theft_records =
    nrow(crime_candidate),

  duplicate_id_groups =
    nrow(duplicate_id_audit),

  consistent_duplicate_ids =
    length(consistent_duplicate_ids),

  consistent_duplicate_records_removed =
    sum(
      duplicate_id_audit$records[
        duplicate_id_audit$distinct_core_records == 1
      ] - 1
    ),

  conflicting_ids =
    length(conflicting_ids),

  conflicting_records_excluded =
    sum(
      duplicate_id_audit$records[
        duplicate_id_audit$distinct_core_records > 1
      ]
    ),

  records_after_duplicate_cleaning =
    nrow(crime_after_duplicate_cleaning)
)

print(
  duplicate_cleaning_check,
  width = Inf
)


# Expected:
# 170 excess duplicate records removed
# 30 conflicting records excluded
# 311,306 records remaining
stopifnot(
  nrow(crime_after_duplicate_cleaning) == 311306
)


# ------------------------------------------------------------
# 9. Define Greater London LSOAs
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


postcode_lookup <- read_csv(
  lookup_file,
  show_col_types = FALSE
) %>%
  clean_names()


london_lsoa_codes <- postcode_lookup %>%
  filter(
    ladnm %in% london_boroughs
  ) %>%
  distinct(lsoa21cd) %>%
  pull(lsoa21cd)


# Dissertation study area = 4,994 LSOAs
stopifnot(
  length(london_lsoa_codes) == 4994
)


# ------------------------------------------------------------
# 10. Remove missing and non-London LSOA records
# ------------------------------------------------------------

location_cleaning_check <- tibble(

  records_after_duplicate_cleaning =
    nrow(crime_after_duplicate_cleaning),

  missing_lsoa =
    sum(
      is.na(
        crime_after_duplicate_cleaning$lsoa_code
      ) |
        crime_after_duplicate_cleaning$lsoa_code == ""
    ),

  outside_london =
    sum(
      !is.na(
        crime_after_duplicate_cleaning$lsoa_code
      ) &
        crime_after_duplicate_cleaning$lsoa_code != "" &
        !crime_after_duplicate_cleaning$lsoa_code %in%
        london_lsoa_codes
    )
)

print(
  location_cleaning_check,
  width = Inf
)


# Expected:
# missing LSOA = 666
# outside Greater London = 532
stopifnot(
  location_cleaning_check$missing_lsoa == 666,
  location_cleaning_check$outside_london == 532
)


crime_clean_london <- crime_after_duplicate_cleaning %>%

  filter(
    !is.na(lsoa_code),
    lsoa_code != "",
    lsoa_code %in% london_lsoa_codes
  )


# Final offence-level total
stopifnot(
  nrow(crime_clean_london) == 310108
)


# ------------------------------------------------------------
# 11. Check final theft-category totals
# ------------------------------------------------------------

category_check <- crime_clean_london %>%
  count(
    crime_type,
    name = "offences"
  ) %>%
  arrange(desc(offences))

print(
  category_check,
  n = Inf
)


# Expected dissertation totals:
expected_category_totals <- tibble(
  crime_type = c(
    "Other theft",
    "Theft from the person",
    "Shoplifting",
    "Bicycle theft"
  ),

  expected = c(
    111906,
    98547,
    84123,
    15532
  )
)


category_validation <- category_check %>%
  left_join(
    expected_category_totals,
    by = "crime_type"
  ) %>%
  mutate(
    matches_expected =
      offences == expected
  )

print(
  category_validation,
  n = Inf
)

stopifnot(
  all(category_validation$matches_expected)
)


# ------------------------------------------------------------
# 12. Aggregate offences to LSOA level
# ------------------------------------------------------------

crime_lsoa_2024_cleaned <- crime_clean_london %>%

  group_by(lsoa_code) %>%

  summarise(

    theft_count = n(),

    shoplifting_count =
      sum(
        crime_type == "Shoplifting"
      ),

    theft_from_person_count =
      sum(
        crime_type == "Theft from the person"
      ),

    bicycle_theft_count =
      sum(
        crime_type == "Bicycle theft"
      ),

    other_theft_count =
      sum(
        crime_type == "Other theft"
      ),

    .groups = "drop"
  )


# ------------------------------------------------------------
# 13. Final validation
# ------------------------------------------------------------

final_check <- crime_lsoa_2024_cleaned %>%

  summarise(

    lsoas_with_recorded_theft = n(),

    total_theft =
      sum(theft_count),

    total_shoplifting =
      sum(shoplifting_count),

    total_theft_from_person =
      sum(theft_from_person_count),

    total_bicycle_theft =
      sum(bicycle_theft_count),

    total_other_theft =
      sum(other_theft_count)
  )

print(
  final_check,
  width = Inf
)

stopifnot(
  final_check$total_theft == 310108,
  final_check$total_shoplifting == 84123,
  final_check$total_theft_from_person == 98547,
  final_check$total_bicycle_theft == 15532,
  final_check$total_other_theft == 111906
)


# ------------------------------------------------------------
# 14. Save cleaned LSOA-level crime dataset
# ------------------------------------------------------------

write_csv(
  crime_lsoa_2024_cleaned,
  file.path(
    processed_dir,
    "crime_lsoa_2024_cleaned.csv"
  )
)


message(
  "01_crime_cleaning_2024.R completed successfully."
)
