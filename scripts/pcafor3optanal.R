# ============================================
# PCA Plot for 3-Feature Clustering (New Step)
# ============================================

library(ggplot2)
library(tidyverse)

# 1. Load data
df3 <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_3features.csv")

# 2. Select features
pca_features <- df3 %>% select(prog_z, reread_z, lookback_z)  # these are the 3 standardized features
pca_result <- prcomp(pca_features, center = TRUE, scale. = TRUE)

# 3. Add cluster labels
pca_df <- as.data.frame(pca_result$x)
pca_df$Cluster <- as.factor(df3$Cluster_H3F)

# 4. Plot PC1 vs PC2
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PCA of 3-Feature Clustering",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")
  ) +
  scale_color_brewer(palette = "Set1")

# 5. Save
ggsave("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/pca_3feature_cluster.png",
       width = 7, height = 5, dpi = 300)
