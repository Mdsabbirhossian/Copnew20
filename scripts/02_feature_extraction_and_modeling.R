# =====================================================
# Script: 02_feature_extraction_and_modeling.R (FINAL)
# Author: Farhana Kabir
# Purpose: Prepare z-scored fixation features for G-group assignment
#          based on Hyönä & Nurminen (2006) using MECO-L2 Wave 1
# Updated: 2025-06-21 | Paths updated to Copnew20
# =====================================================

# 1. Setup ----
rm(list = ls())
library(tidyverse)
library(psych)

# 2. Load dataset with final features + G-group labels ----
load("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_with_ggroups.rda")  # loads: by.sub_final

# Check object
if (!exists("by.sub_final")) stop("❌ Object 'by.sub_final' not found.")

# Assign to standard name
by.sub_z <- by.sub_final

# 3. Rename fallback (for older datasets) ----
if (all(c("prog", "lookback", "reread") %in% names(by.sub_z))) {
  by.sub_z <- by.sub_z %>%
    rename(
      prog_z     = prog,
      lookback_z = lookback,
      reread_z   = reread
    )
}

# 4. Ensure required columns ----
core_features <- c("prog_z", "lookback_z", "reread_z")
if (!all(core_features %in% colnames(by.sub_z))) {
  stop("❌ Missing z-scored feature(s): ", paste(setdiff(core_features, colnames(by.sub_z)), collapse = ", "))
}
if (!"G_group" %in% colnames(by.sub_z)) {
  stop("❌ Missing group label column: 'G_group'")
}

# 5. Summary stats ----
cat("\n===== Z-Score Summary of Core Features =====\n")
z_stats <- describe(by.sub_z[, core_features])
print(z_stats[, c("mean", "sd", "n")])

# 6. G-group label distribution ----
cat("\n===== G_group Label Distribution (G1, G2, G3) =====\n")
print(table(by.sub_z$G_group))

# 7. Save full dataset ----
write.csv(by.sub_z,
          "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_zscored_with_ggroups.csv",
          row.names = FALSE)

save(by.sub_z,
     file = "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_zscored_with_ggroups.rda")

# 8. Save compact dataset for G-group modeling only ----
clustering_data <- by.sub_z %>%
  select(uniform_id, prog_z, lookback_z, reread_z, G_group)

write.csv(clustering_data,
          "C:/Users/farha/ANLP_project/Copnew20/data/features/for_clustering_zfeatures_only.csv",
          row.names = FALSE)

message("\n✅ Z-scored fixation features (Hyönä-style) exported for G-group modeling.")