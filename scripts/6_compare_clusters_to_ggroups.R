# ======================================================
# Script: 06_compare_clusters_to_ggroups_final.R (Final Version)
# Author: Farhana Kabir
# Purpose: Create Figure 5 - Cluster vs. G-group (Mahalanobis & Cosine)
# ======================================================

library(tidyverse)
library(gridExtra)
library(grid)

# ---- 1. Load and define ----
g_features <- c("prog_z", "reread_z", "lookback_z")

df3 <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_3features.csv")
df8 <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_8features.csv")
df_g <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_with_ggroups.csv")

# Filter out NA clusters
df3 <- df3 %>% filter(!is.na(Cluster_H3F))
df8 <- df8 %>% filter(!is.na(Cluster_8F))
df_g <- df_g %>% filter(!is.na(G_group))

# Define labels
g_labels <- c("GG1\n(Low all)", "GG2\n(High reread/lookback)", "GG3\n(Low prog, High lookback)")

g_centroids <- df_g %>%
  group_by(G_group) %>%
  summarise(across(all_of(g_features), \(x) mean(x, na.rm = TRUE))) %>%
  arrange(G_group) %>%
  mutate(G_group = g_labels)

c3 <- df3 %>%
  group_by(Cluster_H3F) %>%
  summarise(across(all_of(g_features), mean)) %>%
  mutate(Cluster = paste0("Cluster ", Cluster_H3F, " (3-Feature)"))

c8 <- df8 %>%
  group_by(Cluster_8F) %>%
  summarise(across(all_of(g_features), mean)) %>%
  mutate(Cluster = paste0("Cluster ", Cluster_8F, " (8-Feature)"))

centroids <- bind_rows(c3 %>% select(Cluster, all_of(g_features)),
                       c8 %>% select(Cluster, all_of(g_features)))

# ---- 2. Distance functions ----
cosine_dist <- function(a, b) {
  sim <- sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  1 - sim
}

cos_mat <- expand.grid(Cluster = centroids$Cluster, G_group = g_centroids$G_group)
cos_mat$Dist <- mapply(function(c, g) {
  cosine_dist(
    as.numeric(centroids[centroids$Cluster == c, g_features]),
    as.numeric(g_centroids[g_centroids$G_group == g, g_features])
  )
}, cos_mat$Cluster, cos_mat$G_group)

mahal_mat <- expand.grid(Cluster = centroids$Cluster, G_group = g_centroids$G_group)
mahal_mat$Dist <- mapply(function(c, g) {
  mahalanobis(
    x = as.numeric(centroids[centroids$Cluster == c, g_features]),
    center = as.numeric(g_centroids[g_centroids$G_group == g, g_features]),
    cov = diag(3)
  )
}, mahal_mat$Cluster, mahal_mat$G_group)

# ---- 3. Heatmap function ----
plot_heatmap <- function(df, label, title, type = "mahal") {
  df$IsMin <- ave(df$Dist, df$Cluster, FUN = function(x) x == min(x))
  pal <- if (type == "mahal") scale_fill_gradient(low = "#4575b4", high = "#d73027", limits = c(0, 12))
          else scale_fill_gradient(low = "#4575b4", high = "#d73027", limits = c(0, 2))
  
  ggplot(df, aes(G_group, Cluster, fill = Dist)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Dist),
                  fontface = ifelse(IsMin, "bold", "plain"),
                  color = ifelse(IsMin, "black", "white")),
              size = 4) +
    pal +
    scale_color_identity() +
    labs(title = paste0(label, " ", title),
         x = "G-group Centroid (G1–G3)",
         y = "Cluster Centroid (C1–C3)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

p1 <- plot_heatmap(mahal_mat, "(a)", "Mahalanobis Distance", "mahal")
p2 <- plot_heatmap(cos_mat, "(b)", "Cosine Distance", "cos")

# ---- 4. Save all outputs ----
output_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

combined <- grid.arrange(p1, p2, ncol = 2, top = textGrob(
  "Figure 5: Cluster–G-group Distance Comparison\n(Z-scored Hyönä Features: prog_z, reread_z, lookback_z)",
  gp = gpar(fontsize = 16, fontface = "bold")
))
ggsave(file.path(output_dir, "figure5_cluster_ggroup_comparison_final.png"), combined, width = 16, height = 6, dpi = 300)

ggsave(file.path(output_dir, "figure5_mahalanobis_only.png"), p1, width = 6, height = 5, dpi = 300)
ggsave(file.path(output_dir, "figure5_cosine_only.png"), p2, width = 6, height = 5, dpi = 300)


# ---- 5. Save tables ----
write.csv(mahal_mat, file.path(output_dir, "mahalanobis_matrix.csv"), row.names = FALSE)
write.csv(cos_mat,    file.path(output_dir, "cosine_matrix.csv"),     row.names = FALSE)

message("✅ All updated plots and tables saved successfully.")
