# ================================================
# Script: 07_compare_cluster_to_Ggroup_summary.R
# Purpose: Compare 3F vs. 8F cluster alignment to G1–G3 + Visual Summary
# Author: Farhana Kabir
# ================================================

# 1. Setup ----
library(tidyverse)
library(ggpubr)

# 2. Load Precomputed Distance Matrices ----
cosine_df <- read.csv("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/cosine_matrix.csv")
mahal_df  <- read.csv("C:/Users/farha/ANLP_project/Copnew20/outputs/plots/mahalanobis_matrix.csv")

# 3. Add Cluster Type ----
cosine_df$Type <- ifelse(grepl("3-Feature", cosine_df$Cluster), "3F", "8F")
mahal_df$Type <- cosine_df$Type  # Assuming same row order

# 4. Combine ----
dist_df <- cosine_df %>%
  rename(Cosine = Dist) %>%
  select(Cluster, G_group, Cosine, Type) %>%
  left_join(
    mahal_df %>% rename(Mahalanobis = Dist) %>% select(Cluster, G_group, Mahalanobis),
    by = c("Cluster", "G_group")
  )

# 5. Identify Closest G-group ----
dist_df <- dist_df %>%
  group_by(Cluster) %>%
  mutate(
    Closest_G_cos = G_group[which.min(Cosine)],
    Closest_G_mahal = G_group[which.min(Mahalanobis)]
  ) %>%
  ungroup()

# 6. Visualization: Heatmap of Cosine Distance ----
p_cos <- ggplot(dist_df, aes(x = G_group, y = Cluster, fill = Cosine)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Cosine, 2)), color = "black") +
  scale_fill_gradient(low = "#4575b4", high = "#d73027", limits = c(0, 2)) +
  labs(title = "Cosine Distance: Cluster-to-G Group", x = "G-group", y = "Cluster") +
  theme_minimal(base_size = 12)

# 7. Visualization: Heatmap of Mahalanobis Distance ----
p_mahal <- ggplot(dist_df, aes(x = G_group, y = Cluster, fill = Mahalanobis)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Mahalanobis, 2)), color = "black") +
  scale_fill_gradient(low = "#4575b4", high = "#d73027", limits = c(0, 12)) +
  labs(title = "Mahalanobis Distance: Cluster-to-G Group", x = "G-group", y = "Cluster") +
  theme_minimal(base_size = 12)

# 8. Save Plots ----
plot_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
ggsave(file.path(plot_dir, "heatmap_cosine_vs_ggroup.png"), p_cos, width = 7, height = 5, dpi = 300)
ggsave(file.path(plot_dir, "heatmap_mahalanobis_vs_ggroup.png"), p_mahal, width = 7, height = 5, dpi = 300)

# 9. Summary Table ----
summary_table <- dist_df %>%
  group_by(Cluster, Type) %>%
  summarise(
    Closest_G_cos = Closest_G_cos[1],
    Cosine = min(Cosine),
    Closest_G_mahal = Closest_G_mahal[1],
    Mahalanobis = min(Mahalanobis)
  ) %>%
  ungroup() %>%
  arrange(Type, Cluster)

write.csv(summary_table, file.path(plot_dir, "cluster_ggroup_alignment_summary.csv"), row.names = FALSE)
