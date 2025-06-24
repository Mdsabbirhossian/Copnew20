# ======================================================
# Script: 05_compute_cluster_centroids.R
# Author: Farhana Kabir
# Purpose: Compute cluster centroids in G-feature space (prog_z, reread_z, lookback_z)
# ======================================================

library(tidyverse)

# 1. Load uploaded datasets ----
data_raw <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.csv")
data_3feat <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_3features.csv")     # contains Cluster_H3F
data_8feat <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_8features.csv")     # contains Cluster_8F

# 2. Merge cluster labels ----
merged_3 <- inner_join(data_raw, data_3feat %>% select(uniform_id, Cluster_H3F), by = "uniform_id") %>%
  rename(Cluster_3F = Cluster_H3F)

merged_8 <- inner_join(data_raw, data_8feat %>% select(uniform_id, Cluster_8F), by = "uniform_id") %>%
  rename(Cluster_8F = Cluster_8F)

# 3. Define G-features to use ----
g_features <- c("prog_z", "reread_z", "lookback_z")
available_g_features <- g_features[g_features %in% colnames(data_raw)]

if (length(available_g_features) < 2) {
  stop("🚫 Not enough G features available in dataset. Expected: prog_z, reread_z, lookback_z.")
}

cat("✅ Using G-feature(s):", paste(available_g_features, collapse = ", "), "\n")

# 4. Compute cluster centroids ----
centroids_3F <- merged_3 %>%
  group_by(Cluster_3F) %>%
  summarise(across(all_of(available_g_features), mean, na.rm = TRUE)) %>%
  arrange(Cluster_3F)

centroids_8F <- merged_8 %>%
  group_by(Cluster_8F) %>%
  summarise(across(all_of(available_g_features), mean, na.rm = TRUE)) %>%
  arrange(Cluster_8F)

# 5. Save outputs ----
out_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/centroids/"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(centroids_3F,
          file.path(out_dir, "centroids_3feature_clusters.csv"),
          row.names = FALSE)

write.csv(centroids_8F,
          file.path(out_dir, "centroids_8feature_clusters.csv"),
          row.names = FALSE)

cat("\n✅ Cluster centroid calculation completed.\n")
cat("📁 Saved to:\n")
cat("   -", file.path(out_dir, "centroids_3feature_clusters.csv"), "\n")
cat("   -", file.path(out_dir, "centroids_8feature_clusters.csv"), "\n")
