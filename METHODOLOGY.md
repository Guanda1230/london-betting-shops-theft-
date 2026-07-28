# Methodology

## Study design and unit of analysis

The study uses a cross-sectional ecological design to examine the association
between betting-shop concentration and recorded theft across Greater London in
2024. The unit of analysis is the 2021 Lower Layer Super Output Area (LSOA).
The final analytical dataset is expected to contain all 4,994 London LSOAs.

The analysis concerns a broader public and policy issue as well as an academic
one. The spatial concentration of crime affects residents, visitors, businesses
and public services. It is also relevant to crime science and environmental
criminology because those disciplines study how crime is distributed across
places and how land use, routine activities, facilities and opportunity
structures shape that distribution.

## Recorded theft outcome

Street-level Police.uk records for the Metropolitan Police Service and City of
London Police were compiled for January to December 2024. The main outcome is
more precisely described as **selected recorded non-vehicle theft categories**.
It combines:

- Other theft
- Theft from the person
- Shoplifting
- Bicycle theft

Police.uk **Vehicle crime is excluded**. The selected records are aggregated by
2021 LSOA code. Shoplifting and theft from the person are also aggregated
separately for sensitivity analyses.

The code reports raw record counts, records retained after the force/year
restriction, duplicate crime IDs, selected-category records, missing LSOA codes,
and records entering the final aggregation. It also checks the completeness of
the 24 expected police-force-by-month combinations.

Police.uk coordinates are anonymised to nearby map points. The LSOA code in the
download therefore corresponds to the anonymised snap point rather than
necessarily the exact offence location. Offences close to LSOA boundaries may
consequently be assigned to a neighbouring LSOA. This is a source of possible
boundary misclassification.

## Betting-shop exposure

Betting-shop premises are identified from the Gambling Commission premises
licence register. The program retains rows for which `premises_activity`
contains the case-insensitive string `betting`. The exact activity values and
their row counts are exported to
`outputs/audit/betting_activity_filter_values.csv`.

Activity-specific register rows can refer to the same physical premises. The
workflow therefore identifies distinct physical premises using the
premises-licence identifier where available. Missing identifiers are resolved
using a normalised address, trading-name and postcode key. If the register
version contains no recognised licence identifier, the full normalised address
key is used. The applied rule and the number of rows removed are exported to
`outputs/audit/betting_record_flow.csv`.

Postcodes are upper-cased and stripped of spaces, then linked to 2021 LSOAs
using the supplied postcode-to-LSOA lookup. Premises are retained if the linked
local authority is one of the 32 London boroughs or the City of London.

The primary exposure is `betting_shop_count`, the number of distinct betting
premises in each LSOA. Alternative measures are:

- `has_betting_shop`: one if the LSOA contains at least one shop, otherwise
  zero; and
- `betting_shop_density_km2`: betting-shop count divided by LSOA area in square
  kilometres.

The Gambling Commission register is live and historical snapshots may not be
retained. The exact access date must be entered in
`config/source_metadata.csv`. Unless a 2024 snapshot is available, the measure
should be interpreted as exposure captured by the downloaded register snapshot,
not as a definitive historical inventory of betting shops operating throughout
2024.

## Deprivation, population and spatial data

Deprivation measures are extracted from the English Indices of Deprivation
2025 File 7. The workflow uses the named columns for:

- 2021 LSOA code and name;
- Index of Multiple Deprivation score, rank and decile; and
- income deprivation score/rate, rank and decile.

The regression models use the **scores**, not ranks or deciles. Scores retain
more information and support interpretation of relative area-level deprivation.
They do not represent individual deprivation. Overall IMD and income
deprivation are estimated in separate models because income deprivation is a
constituent domain of IMD and simultaneous inclusion could introduce
multicollinearity.

Resident population is taken from the ONS mid-2024 LSOA 2021 estimates.
Population is used to calculate descriptive rates and its natural logarithm is
used as the count-model offset.

The 2021 LSOA boundary file is transformed to British National Grid
(`EPSG:27700`) before areas are calculated in square kilometres. The exact
source filename, layer and original coordinate reference system are exported to
`outputs/audit/context_source_files_and_crs.csv`. The boundary product and
resolution must be confirmed in `config/source_metadata.csv` before release.

## Variable construction

For LSOA \(i\):

\[
\text{Theft rate}_{i} =
\frac{\text{theft count}_{i}}{\text{resident population}_{i}}\times 1{,}000
\]

\[
\text{Betting-shop density}_{i} =
\frac{\text{betting-shop count}_{i}}{\text{LSOA area}_{i}\;(\text{km}^{2})}
\]

\[
\text{Population density}_{i} =
\frac{\text{resident population}_{i}}
{\text{LSOA area}_{i}\;(\text{km}^{2})}
\]

\[
\text{Offset}_{i} = \ln(\text{resident population}_{i})
\]

The code additionally defines `population_density_1000` as population density
divided by 1,000. Using this scaled variable does not change model fit; it makes
the reported incidence-rate ratio correspond to an additional 1,000 residents
per square kilometre.

## Descriptive and correlation analysis

The study reports distributions, central tendency, dispersion, borough-level
summaries, and comparisons between LSOAs with and without betting shops.
Spearman correlations are used because the main count and rate variables are
strongly right-skewed. Correlations are described by their direction and
magnitude; statistical significance does not establish causation.

Maps are produced by the author from the compiled spatial dataset. Pseudo-log
colour scales and central-London insets reduce domination by extreme central
LSOAs while retaining all observations.

## Regression models

Poisson models are estimated first and assessed for overdispersion. Because
theft counts are strongly overdispersed, negative binomial regression is used
for the principal analysis.

The income-deprivation specification is:

\[
\log E(Y_i) =
\beta_0 + \beta_1 B_i + \beta_2 D^{income}_i +
\beta_3 P_i + \log(N_i)
\]

The IMD specification replaces income deprivation with overall IMD score.
Here, \(Y_i\) is the theft count, \(B_i\) is betting-shop count, \(P_i\) is
population density per 1,000 residents/km², and \(N_i\) is resident population.
Exponentiated coefficients are reported as incidence-rate ratios with 95%
confidence intervals.

## Sensitivity analyses

The workflow estimates:

- binary betting-shop presence;
- betting-shop density per square kilometre;
- shoplifting as an alternative outcome;
- theft from the person as an alternative outcome;
- local-authority indicator specifications;
- models excluding the 1% of LSOAs with the highest theft counts; and
- betting-shop count grouped as zero, one, two, or three or more premises.

Local-authority indicators account for systematic differences between London
boroughs, but they are not a panel-data fixed-effects design and cannot remove
unmeasured within-borough variation.

## Spatial diagnostic

Pearson residuals from both principal negative binomial models are tested using
Global Moran’s I. Queen-contiguity neighbours share a boundary or vertex,
weights are row-standardised, and significance is assessed using 9,999
permutations. This procedure tests for remaining residual spatial
autocorrelation. It does **not** estimate a spatial regression or a spatial
count model. Significant positive residual autocorrelation therefore indicates
unexplained geographical clustering and motivates cautious interpretation of
conventional standard errors.

## Interpretation

All estimated relationships are area-level associations. The design does not
establish that betting shops cause theft. Betting-shop concentration may proxy
retail intensity, pedestrian footfall, transport accessibility, nightlife or
other features of commercial centres. The resident-population offset is also
imperfect in central London because non-resident exposure can substantially
exceed the resident population.

