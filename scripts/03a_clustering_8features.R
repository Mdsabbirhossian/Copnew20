# ======================================================
# Script: 03a_clustering_8features.R (FINAL, CLEANED)
# Author: Farhana Kabir
# Purpose: K-means clustering on 8 z-scored gaze features
# Notes:
# - PCA is used for 2D visualization only (not dimensionality reduction)
# - Includes Elbow plot, Scree plot, PCA PC1–PC2, Centroids
# ======================================================

# 1. Setup ----
library(tidyverse)
library(cluster)
library(factoextra)
library(ggpubr)

# 2. Load data ----
features_df <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.csv")

library(corrplot)

# Compute and plot correlation heatmap
corr_matrix <- cor(features_8, use = "complete.obs")

png("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/correlation_matrix_features.png", width = 800, height = 600)
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
         tl.cex = 1.1, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()


# 3. Select z-scored 8 features ----
selected_vars <- c("prog_z", "reread_z", "lookback_z", "skip", "reg.in", "refix", "rate", "comprehension_score")
features_8 <- features_df %>% select(all_of(selected_vars)) %>% drop_na()

# 4. Elbow Method ----
dir.create("C:/Users/farha/ANLP_project/Copnew20/outputs/plots", recursive = TRUE, showWarnings = FALSE)
wss_plot_8 <- fviz_nbclust(features_8, kmeans, method = "wss") +
  labs(title = "Elbow Method: Optimal Clusters (8 Features)") +
  theme_minimal(base_size = 13)
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/elbow_method_8features.png", wss_plot_8, width = 7, height = 5, dpi = 300)

# 5. K-means Clustering ----
set.seed(42)
kmeans_8 <- kmeans(features_8, centers = 3, nstart = 25)

# Assign clusters only to matching rows using row index
features_df$Cluster_8F <- NA
features_df$Cluster_8F[which(complete.cases(features_df[selected_vars]))] <- kmeans_8$cluster

# 6. PCA for Visualization ----
pca_8 <- prcomp(features_8, scale. = TRUE)
pca_df_8 <- as.data.frame(pca_8$x) %>%
  mutate(Cluster = factor(kmeans_8$cluster))

# 6a. Scree Plot ----
scree_8 <- fviz_eig(pca_8, addlabels = TRUE) +
  labs(title = "PCA Scree Plot: 8 Features")
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/pca_scree_8features.png", scree_8, width = 6, height = 4, dpi = 300)

# 6b. PCA Projection: PC1 vs PC2 ----
p12_8 <- ggplot(pca_df_8, aes(PC1, PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "norm", level = 0.68, linetype = "dashed") +
  labs(
    title = "PC1 vs PC2 (8 Features)",
    x = paste0("PC1 (", round(summary(pca_8)$importance[2,1]*100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_8)$importance[2,2]*100, 1), "%)")
  ) +
  theme_minimal(base_size = 13)
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/pca_8features_PC1_PC2.png", p12_8, width = 8, height = 6, dpi = 300)
# --- Additional PCA Projections for 8 Features ---

# (a) PC1 vs PC3
p13_8 <- ggplot(pca_df_8, aes(PC1, PC3, color = Cluster)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "norm", level = 0.68, linetype = "dashed") +
  labs(
    title = "PC1 vs PC3 (8 Features)",
    x = paste0("PC1 (", round(summary(pca_8)$importance[2,1]*100, 1), "%)"),
    y = paste0("PC3 (", round(summary(pca_8)$importance[2,3]*100, 1), "%)")
  ) +
  theme_minimal(base_size = 13)
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/pca_8features_PC1_PC3.png", p13_8, width = 8, height = 6, dpi = 300)

# (b) PC2 vs PC3
p23_8 <- ggplot(pca_df_8, aes(PC2, PC3, color = Cluster)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "norm", level = 0.68, linetype = "dashed") +
  labs(
    title = "PC2 vs PC3 (8 Features)",
    x = paste0("PC2 (", round(summary(pca_8)$importance[2,2]*100, 1), "%)"),
    y = paste0("PC3 (", round(summary(pca_8)$importance[2,3]*100, 1), "%)")
  ) +
  theme_minimal(base_size = 13)
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/pca_8features_PC2_PC3.png", p23_8, width = 8, height = 6, dpi = 300)


# 7. Centroid Plot ----
centroids_8 <- as.data.frame(kmeans_8$centers) %>% mutate(Cluster = paste("C", 1:3))
centroids_long_8 <- pivot_longer(centroids_8, -Cluster)
centroid_plot_8 <- ggplot(centroids_long_8, aes(x = name, y = value, fill = Cluster)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  labs(title = "Cluster Centroids: 8 Features", x = "Feature", y = "Z-scored Value")
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/centroids_8features.png", centroid_plot_8, width = 10, height = 6, dpi = 300)

# 8. Save clustered data ----
write.csv(features_df, "C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_8features.csv", row.names = FALSE)

message("✅ 8-feature clustering complete.")
