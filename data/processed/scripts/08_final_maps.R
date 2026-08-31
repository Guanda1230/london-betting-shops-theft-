
# ============================================================
# 08_final_maps.R
#
# MSc Dissertation:
# Betting-Shop Concentration, Deprivation and Recorded Theft
# in London: A Spatial Crime Analysis
#
# Purpose:
# Produce the three final maps reported in Chapter 4.
#
# Figure 4.1:
# Recorded theft count by London LSOA, 2024
# - log1p display transformation
# - central-London inset
#
# Figure 4.2:
# Betting-shop count by London LSOA
# - categories: 0, 1, 2, 3+
#
# Figure 4.3:
# Index of Multiple Deprivation by London LSOA
# - London-wide IMD quintiles
#
# Input:
# data/processed/final_london_lsoa_dataset.gpkg
#
# Outputs:
# outputs/figures/Figure_4_1_Theft_Count.png
# outputs/figures/Figure_4_2_Betting_Shop_Count.png
# outputs/figures/Figure_4_3_IMD.png
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)


# ------------------------------------------------------------
# 2. Project paths
# ------------------------------------------------------------

spatial_file <- file.path(
  "data",
  "processed",
  "final_london_lsoa_dataset.gpkg"
)

output_dir <- file.path(
  "outputs",
  "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. Load final spatial analytical dataset
# ------------------------------------------------------------

if (!file.exists(spatial_file)) {
  stop(
    paste(
      "final_london_lsoa_dataset.gpkg was not found.",
      "Run scripts/02_data_preparation.R first."
    )
  )
}


london_sf <- st_read(
  spatial_file,
  quiet = TRUE
)


# ------------------------------------------------------------
# 4. Validate spatial dataset
# ------------------------------------------------------------

required_variables <- c(
  "lsoa_code",
  "theft_count",
  "betting_shop_count",
  "imd_score"
)

missing_variables <- setdiff(
  required_variables,
  names(london_sf)
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
  nrow(london_sf) == 4994
)

stopifnot(
  n_distinct(london_sf$lsoa_code) == 4994
)

stopifnot(
  sum(
    london_sf$theft_count,
    na.rm = TRUE
  ) == 310108
)

stopifnot(
  sum(
    london_sf$betting_shop_count,
    na.rm = TRUE
  ) == 1256
)


# ------------------------------------------------------------
# 5. Prepare mapping variables
# ------------------------------------------------------------
#
# British National Grid is used so that the central-London
# inset can be specified using projected coordinates.
#
# The theft transformation and IMD quintiles below are used
# only for visualisation. Statistical models use the original
# continuous variables.

map_sf <- london_sf %>%

  st_transform(
    27700
  ) %>%

  mutate(

    # Figure 4.1:
    # log1p display transformation only
    theft_log =
      log1p(
        theft_count
      ),

    # Figure 4.2:
    # betting-shop categories
    betting_shop_cat =
      case_when(
        betting_shop_count == 0 ~ "0",
        betting_shop_count == 1 ~ "1",
        betting_shop_count == 2 ~ "2",
        betting_shop_count >= 3 ~ "3+"
      ),

    betting_shop_cat =
      factor(
        betting_shop_cat,
        levels = c(
          "0",
          "1",
          "2",
          "3+"
        )
      ),

    # Figure 4.3:
    # quintiles calculated across the 4,994 London LSOAs
    imd_quintile =
      ntile(
        imd_score,
        5
      ),

    imd_quintile =
      factor(
        imd_quintile,
        levels = 1:5,
        labels = c(
          "Q1 (lowest)",
          "Q2",
          "Q3",
          "Q4",
          "Q5 (highest)"
        )
      )
  )


# ------------------------------------------------------------
# 6. Common map theme
# ------------------------------------------------------------

map_theme <- theme_void() +

  theme(

    legend.position =
      "right",

    legend.title =
      element_text(
        size = 11
      ),

    legend.text =
      element_text(
        size = 9
      ),

    plot.title =
      element_text(
        size = 14,
        face = "bold"
      ),

    plot.caption =
      element_text(
        size = 9
      ),

    plot.caption.position =
      "plot"
  )


# ============================================================
# FIGURE 4.1
# RECORDED THEFT COUNT
# ============================================================


# ------------------------------------------------------------
# 7. Theft-count legend values
# ------------------------------------------------------------

theft_break_values <- c(
  0,
  10,
  25,
  50,
  100,
  250,
  500,
  1000,
  5000,
  12000
)


# ------------------------------------------------------------
# 8. Main theft map
# ------------------------------------------------------------

theft_map <- ggplot(
  map_sf
) +

  geom_sf(
    aes(
      fill = theft_log
    ),
    colour = NA
  ) +

  scale_fill_viridis_c(

    breaks =
      log1p(
        theft_break_values
      ),

    labels =
      format(
        theft_break_values,
        big.mark = ","
      ),

    name =
      "Recorded theft\ncount"
  ) +

  labs(

    title =
      "Recorded Theft Count by London LSOA, 2024",

    caption = paste(
      "Note. A log1p display scale is used to improve readability",
      "of the highly skewed theft counts. Statistical analyses use",
      "the original untransformed counts."
    )
  ) +

  map_theme


# ------------------------------------------------------------
# 9. Central-London inset
# ------------------------------------------------------------
#
# Coordinates are British National Grid metres.
# These limits were used in the final dissertation map.

central_xlim <- c(
  517000,
  536000
)

central_ylim <- c(
  175000,
  186000
)


theft_inset <- ggplot(
  map_sf
) +

  geom_sf(
    aes(
      fill = theft_log
    ),
    colour = NA
  ) +

  scale_fill_viridis_c(
    guide = "none"
  ) +

  coord_sf(
    xlim = central_xlim,
    ylim = central_ylim,
    expand = FALSE
  ) +

  theme_void() +

  theme(
    panel.border =
      element_rect(
        fill = NA,
        linewidth = 0.8
      )
  )


# ------------------------------------------------------------
# 10. Add inset to Figure 4.1
# ------------------------------------------------------------

figure_4_1 <- theft_map +

  inset_element(
    theft_inset,
    left = 0.54,
    bottom = 0.53,
    right = 0.95,
    top = 0.94
  )


print(
  figure_4_1
)


# ------------------------------------------------------------
# 11. Save Figure 4.1
# ------------------------------------------------------------

ggsave(

  filename = file.path(
    output_dir,
    "Figure_4_1_Theft_Count.png"
  ),

  plot =
    figure_4_1,

  width =
    10,

  height =
    7,

  dpi =
    300,

  bg =
    "white"
)


# ============================================================
# FIGURE 4.2
# BETTING-SHOP COUNT
# ============================================================


# ------------------------------------------------------------
# 12. Betting-shop map
# ------------------------------------------------------------

figure_4_2 <- ggplot(
  map_sf
) +

  geom_sf(
    aes(
      fill = betting_shop_cat
    ),
    colour = NA
  ) +

  scale_fill_viridis_d(
    name =
      "Betting shops"
  ) +

  labs(

    title =
      "Betting-Shop Count by London LSOA",

    caption =
      paste(
        "Note. Betting-shop counts are displayed as",
        "0, 1, 2 and 3+ premises per LSOA."
      )
  ) +

  map_theme


print(
  figure_4_2
)


# ------------------------------------------------------------
# 13. Save Figure 4.2
# ------------------------------------------------------------

ggsave(

  filename = file.path(
    output_dir,
    "Figure_4_2_Betting_Shop_Count.png"
  ),

  plot =
    figure_4_2,

  width =
    10,

  height =
    7,

  dpi =
    300,

  bg =
    "white"
)


# ============================================================
# FIGURE 4.3
# INDEX OF MULTIPLE DEPRIVATION
# ============================================================


# ------------------------------------------------------------
# 14. IMD quintile map
# ------------------------------------------------------------

figure_4_3 <- ggplot(
  map_sf
) +

  geom_sf(
    aes(
      fill = imd_quintile
    ),
    colour = NA
  ) +

  scale_fill_viridis_d(
    name =
      "IMD quintile"
  ) +

  labs(

    title =
      "Index of Multiple Deprivation by London LSOA",

    caption = paste(
      "Note. IMD scores are displayed as quintiles across the",
      "4,994 Greater London LSOAs; higher quintiles indicate",
      "greater deprivation."
    )
  ) +

  map_theme


print(
  figure_4_3
)


# ------------------------------------------------------------
# 15. Save Figure 4.3
# ------------------------------------------------------------

ggsave(

  filename = file.path(
    output_dir,
    "Figure_4_3_IMD.png"
  ),

  plot =
    figure_4_3,

  width =
    10,

  height =
    7,

  dpi =
    300,

  bg =
    "white"
)


# ------------------------------------------------------------
# 16. Final check
# ------------------------------------------------------------

expected_figures <- c(

  "Figure_4_1_Theft_Count.png",

  "Figure_4_2_Betting_Shop_Count.png",

  "Figure_4_3_IMD.png"
)


figure_files <- file.path(
  output_dir,
  expected_figures
)


stopifnot(
  all(
    file.exists(
      figure_files
    )
  )
)


message(
  "08_final_maps.R completed successfully."
)
