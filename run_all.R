# Run the complete analytical workflow from the project root.

source(file.path("code", "00_setup.R"))

scripts <- c(
  "01_prepare_police_data.R",
  "02_prepare_betting_shops.R",
  "03_prepare_context_data.R",
  "04_compile_dataset.R",
  "05_descriptive_analysis.R",
  "06_mapping.R",
  "07_main_models.R",
  "08_sensitivity_checks.R",
  "09_spatial_diagnostics.R",
  "10_build_audit_report.R"
)

for (script in scripts) {
  message("\n--- Running ", script, " ---")
  source(file.path(CODE_DIR, script), local = globalenv())
}

message("\nAnalysis completed successfully.")

