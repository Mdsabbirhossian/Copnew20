library(tidyverse)
library(ggpubr)

# Path where the files are stored
base_path <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"

# Use original names for labels, but normalize for filenames
original_features <- c("skip", "reg.in", "refix", "rate", "comprehension")

for (feature in original_features) {
  message("\n📦 Processing feature: ", feature)

  # Safe filename version: remove special characters
  safe_name <- gsub("[^[:alnum:]_]", "", feature)

  # Construct filenames using safe version
  mahal_file <- file.path(base_path, paste0("mahalanobis_3plus_", safe_name, ".csv"))
  cosine_file <- file.path(base_path, paste0("cosine_3plus_", safe_name, ".csv"))

  if (!file.exists(mahal_file) || !file.exists(cosine_file)) {
    warning("❌ One or both files not found for feature: ", feature)
    next
  }

  df_mahal <- read.csv(mahal_file, row.names = 1, check.names = FALSE)
  df_cosine <- read.csv(cosine_file, row.names = 1, check.names = FALSE)

  # Reshape to long format
  mahal_long <- df_mahal %>%
    rownames_to_column("Cluster") %>%
    pivot_longer(-Cluster, names_to = "GGroup", values_to = "Dist")

  cosine_long <- df_cosine %>%
    rownames_to_column("Cluster") %>%
    pivot_longer(-Cluster, names_to = "GGroup", values_to = "Dist")

  # Skip if empty
  if (all(is.na(mahal_long$Dist))) {
    warning("⚠️ Mahalanobis distances are all NA for: ", feature)
    next
  }
  if (all(is.na(cosine_long$Dist))) {
    warning("⚠️ Cosine distances are all NA for: ", feature)
    next
  }

  # Plot Mahalanobis
  p1 <- ggplot(mahal_long, aes(x = GGroup, y = Cluster, fill = Dist)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Dist, 2)), size = 4) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = median(mahal_long$Dist, na.rm = TRUE),
                         na.value = "grey90") +
    labs(title = paste0("(a) Mahalanobis Distance: 3 + ", feature),
         x = "G-group Centroid (G1–G3)", y = "Cluster Centroid (C1–C3)") +
    theme_minimal(base_size = 12)

  # Plot Cosine
  p2 <- ggplot(cosine_long, aes(x = GGroup, y = Cluster, fill = Dist)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Dist, 2)), size = 4) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = median(cosine_long$Dist, na.rm = TRUE),
                         limits = c(0, 2), na.value = "grey90") +
    labs(title = paste0("(b) Cosine Distance: 3 + ", feature),
         x = "G-group Centroid (G1–G3)", y = "Cluster Centroid (C1–C3)") +
    theme_minimal(base_size = 12)

  combined_plot <- ggarrange(p1, p2, ncol = 2)
  final_title <- paste0("Figure: Cluster–G-group Distance Comparison (3 Features + ", feature, ")")
  combined_plot <- annotate_figure(combined_plot, top = text_grob(final_title, face = "bold"))

  out_path <- file.path(base_path, paste0("figure_cluster_ggroup_3plus_", safe_name, ".png"))
  ggsave(out_path, plot = combined_plot, width = 14, height = 6, dpi = 300)

  message("✅ Plot saved for feature: ", feature)
}
