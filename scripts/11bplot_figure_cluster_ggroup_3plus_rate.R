# ====================================================================
# Script: plot_figure_cluster_ggroup_3plus_rate.R
# Purpose: Plot heatmaps of Mahalanobis & Cosine distance matrices
# ====================================================================

library(tidyverse)
library(ggpubr)

# 1. File paths
base_path <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
mahal_file <- file.path(base_path, "mahalanobis_3plus_rate.csv")
cosine_file <- file.path(base_path, "cosine_3plus_rate.csv")

# 2. Load and check
df_mahal <- read.csv(mahal_file, row.names = 1, check.names = FALSE)
df_cosine <- read.csv(cosine_file, row.names = 1, check.names = FALSE)

# 3. Long format
mahal_long <- df_mahal %>%
  rownames_to_column("Cluster") %>%
  pivot_longer(-Cluster, names_to = "GGroup", values_to = "Dist") %>%
  drop_na()

cosine_long <- df_cosine %>%
  rownames_to_column("Cluster") %>%
  pivot_longer(-Cluster, names_to = "GGroup", values_to = "Dist") %>%
  drop_na()

# 4. Plot Mahalanobis
p1 <- ggplot(mahal_long, aes(x = GGroup, y = Cluster, fill = Dist)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Dist, 2)), size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = median(mahal_long$Dist, na.rm = TRUE)) +
  labs(
    title = "(a) Mahalanobis Distance: 3 + rate",
    x = "G-group Centroid (G1–G3)",
    y = "Cluster Centroid (C1–C3)"
  ) +
  theme_minimal(base_size = 13)

# 5. Plot Cosine
p2 <- ggplot(cosine_long, aes(x = GGroup, y = Cluster, fill = Dist)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Dist, 2)), size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = median(cosine_long$Dist, na.rm = TRUE),
                       limits = c(0, 2)) +
  labs(
    title = "(b) Cosine Distance: 3 + rate",
    x = "G-group Centroid (G1–G3)",
    y = "Cluster Centroid (C1–C3)"
  ) +
  theme_minimal(base_size = 13)

# 6. Combine and save
combined_plot <- ggarrange(p1, p2, ncol = 2)
annotated_plot <- annotate_figure(
  combined_plot,
  top = text_grob("Figure: Cluster–G-group Distance Comparison (3 Features + Rate)", face = "bold", size = 14)
)

ggsave(file.path(base_path, "figure_cluster_ggroup_3plus_rate.png"),
       plot = annotated_plot, width = 14, height = 6, dpi = 300)

message("✅ Distance heatmap for 3+rate saved.")
