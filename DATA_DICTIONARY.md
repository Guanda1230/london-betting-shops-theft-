# Data Dictionary

The public compiled CSV contains one row per 2021 London LSOA. Geometry is
excluded from the CSV; the reproducible local workflow can also create a
GeoPackage.

| Variable | Type | Definition |
|---|---|---|
| `lsoa_code` | Character | 2021 LSOA code |
| `lsoa_name` | Character | 2021 LSOA name |
| `local_authority` | Character | London borough or City of London |
| `theft_count` | Integer | Count of other theft, theft from the person, shoplifting and bicycle theft records assigned to the LSOA in 2024; vehicle crime excluded |
| `theft_rate_1000` | Numeric | `theft_count / population × 1,000` |
| `shoplifting_count` | Integer | Shoplifting records assigned to the LSOA in 2024 |
| `shoplifting_rate_1000` | Numeric | `shoplifting_count / population × 1,000` |
| `theft_from_person_count` | Integer | Theft-from-the-person records assigned to the LSOA in 2024 |
| `theft_from_person_rate_1000` | Numeric | `theft_from_person_count / population × 1,000` |
| `betting_shop_count` | Integer | Distinct physical betting premises assigned to the LSOA |
| `has_betting_shop` | Integer | 1 if `betting_shop_count > 0`; otherwise 0 |
| `betting_shops_1000` | Numeric | `betting_shop_count / population × 1,000` |
| `betting_shop_density_km2` | Numeric | `betting_shop_count / area_km2` |
| `imd_score` | Numeric | Overall Index of Multiple Deprivation 2025 score; higher values indicate greater area-level deprivation |
| `imd_rank` | Integer | National IMD rank; 1 is most deprived |
| `imd_decile` | Integer | National IMD decile; 1 is most deprived |
| `income_score` | Numeric | Income deprivation score/rate; higher values indicate greater area-level income deprivation |
| `income_rank` | Integer | National income-deprivation rank; 1 is most deprived |
| `income_decile` | Integer | National income-deprivation decile; 1 is most deprived |
| `population` | Integer | ONS mid-2024 resident population estimate |
| `area_km2` | Numeric | LSOA area in square kilometres, calculated in EPSG:27700 |
| `population_density` | Numeric | Residents per square kilometre |
| `population_density_1000` | Numeric | Population density divided by 1,000; regression scaling variable |
| `log_population` | Numeric | Natural logarithm of resident population; count-model offset |

## Measurement cautions

- Police.uk LSOA assignments reflect anonymised map points and may differ from
  exact offence locations.
- Betting-shop exposure reflects the downloaded live-register snapshot unless
  a documented historical snapshot is used.
- Deprivation variables describe areas, not individuals.
- Resident population does not capture commuters, tourists or other transient
  populations.

