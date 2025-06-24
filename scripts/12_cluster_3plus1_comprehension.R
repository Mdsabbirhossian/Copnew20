# =========================================================
# Script: 12_cluster_3plus1_comprehension.R
# Purpose: K-means clustering with prog_z, reread_z, lookback_z + comprehension_score
# Author: Farhana Kabir | Date: 2025-06-21
# =========================================================

# 1. Load Libraries ----
library(tidyverse)
library(cluster)
library(factoextra)
library(ggpubr)

# 2. Load Datasets ----
df <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.csv")
df_meta <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_with_ggroups.csv")

# 3. Select Features and Clean ----
features_raw <- df %>% select(uniform_id, prog_z, reread_z, lookback_z, comprehension_score)
features <- features_raw %>%
  filter(if_all(-uniform_id, ~ !is.na(.) & is.finite(.)))  # remove rows with NA or non-finite

# 4. Run K-means Clustering (K = 3) ----
set.seed(42)
k3_model <- kmeans(features %>% select(-uniform_id), centers = 3, nstart = 25)
features$Cluster_3P1_comprehension <- factor(k3_model$cluster)

# 5. Merge with G-group Info ----
df_full <- features %>%
  left_join(df_meta %>% select(uniform_id, G_group), by = "uniform_id")

# 6. Filter again to avoid NA in lookback_z before centroid calculations ----
df_filtered <- df_full %>%
  filter(if_all(c("prog_z", "reread_z", "lookback_z"), ~ !is.na(.) & is.finite(.)))

# 7. Compute Cluster Centroids (in G-space) ----
cluster_centroids <- df_filtered %>%
  group_by(Cluster_3P1_comprehension) %>%
  summarise(across(c(prog_z, reread_z, lookback_z), mean), .groups = "drop")

# 8. Compute G-Group Centroids ----
ggroup_centroids <- df_filtered %>%
  group_by(G_group) %>%
  summarise(across(c(prog_z, reread_z, lookback_z), mean), .groups = "drop")

# 9. Compute Mahalanobis and Cosine Distances ----
mahal_dist <- matrix(NA, nrow = 3, ncol = 3)
cosine_dist <- matrix(NA, nrow = 3, ncol = 3)

for (i in 1:3) {
  for (j in 1:3) {
    x <- as.numeric(cluster_centroids[i, 2:4])
    y <- as.numeric(ggroup_centroids[j, 2:4])
    mahal_dist[i, j] <- mahalanobis(x, center = y, cov = diag(3))
    cosine_dist[i, j] <- 1 - sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
  }
}

rownames(mahal_dist) <- paste0("C", 1:3, "_3+compr")
colnames(mahal_dist) <- paste0("GG", 1:3)
rownames(cosine_dist) <- paste0("C", 1:3, "_3+compr")
colnames(cosine_dist) <- paste0("GG", 1:3)

# 10. Save Distance Matrices ----
write.csv(mahal_dist, "C:/Users/farha/ANLP_project/Copnew20/outputs/plots/mahalanobis_3plus_comprehension.csv")
write.csv(cosine_dist, "C:/Users/farha/ANLP_project/Copnew20/outputs/plots/cosine_3plus_comprehension.csv")

# 11. PCA Plots: PC1–PC2, PC1–PC3, PC2–PC3
pca <- prcomp(features %>% select(-uniform_id, -Cluster_3P1_comprehension), center = TRUE, scale. = TRUE)
pca_df <- as.data.frame(pca$x)
pca_df$Cluster <- features$Cluster_3P1_comprehension

# (a) PC1 vs PC2
p1 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "(a) PC1 vs PC2: 3 + comprehension")


# (b) PC1 vs PC3
p2 <- ggplot(pca_df, aes(x = PC1, y = PC3, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "(b) PC1 vs PC3: 3 + comprehension")



# (c) PC2 vs PC3
p3 <- ggplot(pca_df, aes(x = PC2, y = PC3, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "(c) PC2 vs PC3: 3 + comprehension")


# Save all plots
plot_path <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
ggsave(file.path(plot_path, "pca_3plus_comprehension_PC1_PC2.png"), p1, width = 6, height = 5, dpi = 300)
ggsave(file.path(plot_path, "pca_3plus_comprehension_PC1_PC3.png"), p2, width = 6, height = 5, dpi = 300)
ggsave(file.path(plot_path, "pca_3plus_comprehension_PC2_PC3.png"), p3, width = 6, height = 5, dpi = 300)

# 12. Done ----
message("✅ Clustering + G-alignment for 3 + comprehension_score completed.")
