# Betting Shops, Deprivation and Recorded Theft in London

This repository contains the compiled analytical workflow for the dissertation
**“Betting Shop Density, Deprivation and Recorded Theft in London: A Spatial
Crime Analysis.”** The study examines area-level associations across 4,994
London Lower Layer Super Output Areas (LSOAs) in 2024.

The repository is designed to make the analysis transparent and reproducible.
It contains annotated R code, a methodology summary, a data dictionary, data
audit outputs, model results, and the compiled non-spatial analytical dataset.

## Research scope

- **Unit of analysis:** 2021 LSOAs in Greater London
- **Study period:** January–December 2024
- **Primary outcome:** count of selected recorded non-vehicle theft categories
- **Selected categories:** other theft, theft from the person, shoplifting, and
  bicycle theft
- **Explicit exclusion:** Police.uk “Vehicle crime”
- **Primary exposure:** number of betting shops in each LSOA
- **Main models:** negative binomial regressions with a log-population offset
- **Deprivation specifications:** income deprivation and overall IMD entered in
  separate models

## Repository structure

london-betting-shops-theft/
│
├── README.md
├── METHODOLOGY.md
├── DATA_DICTIONARY.md
├── DATA_LICENSES.md
├── LICENSE
├── london-betting-shops-theft.Rproj
├── run_all.R
│
├── scripts/
│   ├── 01_data_cleaning.R
│   ├── 02_build_dataset.R
│   ├── 03_descriptive_analysis.R
│   ├── 04_main_models.R
│   ├── 05_sensitivity_analysis.R
│   ├── 06_spatial_diagnostics.R
│   └── 07_maps.R
│
├── data/
│   ├── processed/
│   │   ├── crime_lsoa_2024_cleaned.csv
│   │   ├── betting_lsoa.csv
│   │   ├── deprivation_lsoa.csv
│   │   ├── population_lsoa_2024.csv
│   │   └── final_london_lsoa_dataset_cleaned.csv
│   │
│   └── README.md
│
├── outputs/
│   ├── tables/
│   └── figures/
│
└── .gitignore
```

## Reproducing the analysis

1. Install R and the required packages:

   ```r
   source("code/install_packages.R")
   ```

2. Place source files under `data/raw` using the layout described in
   `data/raw/README.md`.

3. Complete the exact source access dates and boundary metadata in
   `config/source_metadata.csv`.

4. Open `london-betting-shops-theft.Rproj`, then run:

   ```r
   source("run_all.R")
   ```

If source files are stored outside the repository, set the data root before
running:

```r
Sys.setenv(
  LONDON_BETTING_DATA_ROOT = "C:/Users/your-name/Desktop/dataset"
)
source("run_all.R")
```

## Data auditing

The workflow produces auditable records for:

- the number of source records at each Police.uk processing stage;
- completeness of all 24 force-month combinations;
- duplicated Police.uk crime IDs;
- missing LSOA codes;
- exact Gambling Commission activity values selected by the betting filter;
- the physical-premises deduplication rule;
- postcode-to-LSOA matching;
- source filenames, boundary layer, and coordinate reference system;
- missing values and totals in the compiled LSOA dataset; and
- the computing environment used to run the analysis.

These outputs are written to `outputs/audit`. A concise public summary is
written to `data/processed/data_audit_summary.csv`.

## Important measurement notes

Police.uk street coordinates are anonymised to nearby map points. The attached
LSOA therefore corresponds to an anonymised snap point and may not be the exact
offence LSOA, particularly close to LSOA boundaries.

The Gambling Commission premises register is a live register. A download made
after 2024 may not reproduce betting-shop exposure during 2024. The exact
download date must therefore be reported and the exposure interpreted as a
register snapshot rather than a verified historical inventory.

The residual Moran’s I analysis is a diagnostic test for remaining spatial
autocorrelation. It is not a spatial regression or spatial count model.

## Main limitations

The analysis is cross-sectional and cannot establish causality. Betting-shop
count may proxy unmeasured commercial activity, retail footfall, transport
accessibility, or land use. Resident population is also an imperfect exposure
measure for theft in central London, where daytime and visitor populations can
greatly exceed the number of residents. Significant residual spatial
autocorrelation indicates that conventional standard errors should be
interpreted cautiously.

## Dissertation link text

The following sentence can be included in the dissertation after the repository
has been uploaded:

> The compiled analytical dataset, annotated R code, data-audit outputs and
> accompanying methodological documentation are available at:
> **[INSERT PERMANENT GITHUB REPOSITORY URL]**.

## Reproducibility status

The code was reorganised from the original analysis scripts and checked
structurally. A complete end-to-end rerun requires the original source files.
Before public release, confirm all `TO_CONFIRM` values in
`config/source_metadata.csv` and rerun `run_all.R`.

