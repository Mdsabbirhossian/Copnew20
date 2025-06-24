# =============================================
# Script: 08_cluster_3plus1_skip_analysis.R
# Purpose: Clustering with prog_z, reread_z, lookback_z + skip
# =============================================

library(tidyverse)
library(cluster)
library(factoextra)
library(ggpubr)

# 1. Load data
df <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.csv")
df_meta <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_with_ggroups.csv")

# 2. Select features
features <- df %>% select(prog_z, reread_z, lookback_z, skip)

# 3. Remove rows with NA or infinite values
clean_idx <- which(rowSums(is.na(features) | !is.finite(as.matrix(features))) == 0)
features_clean <- features[clean_idx, ]
df_clean <- df[clean_idx, ]
df_meta_clean <- df_meta[clean_idx, ]

# 4. K-means clustering
set.seed(42)
k3_model <- kmeans(features_clean, centers = 3, nstart = 25)
df_clean$Cluster_3P1_skip <- factor(k3_model$cluster)

# 5. Merge G-group info
df_clean$uniform_id <- df_meta_clean$uniform_id
df_full <- df_clean %>%
  left_join(df_meta_clean %>% select(uniform_id, G_group), by = "uniform_id")

# 6. Compute cluster centroids (3D: prog_z, reread_z, lookback_z)
cluster_centroids <- df_clean %>%
  group_by(Cluster_3P1_skip) %>%
  summarise(across(c(prog_z, reread_z, lookback_z), mean), .groups = "drop")

ggroup_centroids <- df_full %>%
  group_by(G_group) %>%
  summarise(across(c(prog_z, reread_z, lookback_z), mean), .groups = "drop")

# 7. Distance matrices
mahal_dist <- matrix(NA, 3, 3)
cosine_dist <- matrix(NA, 3, 3)

for (i in 1:3) {
  for (j in 1:3) {
    x <- as.numeric(cluster_centroids[i, 2:4])
    y <- as.numeric(ggroup_centroids[j, 2:4])
    
    if (any(is.na(c(x, y))) || any(!is.finite(c(x, y)))) {
      mahal_dist[i, j] <- NA
      cosine_dist[i, j] <- NA
    } else {
      mahal_dist[i, j] <- mahalanobis(x, center = y, cov = diag(3))
      cosine_dist[i, j] <- 1 - sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
    }
  }
}

rownames(mahal_dist) <- paste0("C", 1:3, "_3+skip")
colnames(mahal_dist) <- paste0("GG", 1:3)
rownames(cosine_dist) <- paste0("C", 1:3, "_3+skip")
colnames(cosine_dist) <- paste0("GG", 1:3)

# 8. Save distance matrices
out_path <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
write.csv(mahal_dist, file.path(out_path, "mahalanobis_3plus_skip.csv"))
write.csv(cosine_dist, file.path(out_path, "cosine_3plus_skip.csv"))

# 9. PCA plots (PC1 vs PC2 already done, now add PC1 vs PC3 and PC2 vs PC3)
pca <- prcomp(features_clean, center = TRUE, scale. = TRUE)
pca_df <- as.data.frame(pca$x)
pca_df$Cluster <- df_clean$Cluster_3P1_skip  # or replace with your clustering label

# PC1 vs PC2
p1 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "(a) PC1 vs PC2: 3 + skip", x = "PC1", y = "PC2") +
  scale_color_brewer(palette = "Dark2")

ggsave(file.path(out_path, "pca_3plus_skip_PC1_PC2.png"), p1, width = 6, height = 5, dpi = 300)

# PC1 vs PC3
p2 <- ggplot(pca_df, aes(x = PC1, y = PC3, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "(b) PC1 vs PC3: 3 + skip", x = "PC1", y = "PC3") +
  scale_color_brewer(palette = "Dark2")

ggsave(file.path(out_path, "pca_3plus_skip_PC1_PC3.png"), p2, width = 6, height = 5, dpi = 300)

# PC2 vs PC3
p3 <- ggplot(pca_df, aes(x = PC2, y = PC3, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "norm", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "(c) PC2 vs PC3: 3 + skip", x = "PC2", y = "PC3") +
  scale_color_brewer(palette = "Dark2")

ggsave(file.path(out_path, "pca_3plus_skip_PC2_PC3.png"), p3, width = 6, height = 5, dpi = 300)

message("✅ PCA plots with ellipses saved for 3 + skip")
