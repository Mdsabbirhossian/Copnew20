# ======================================================
# Script: 04_cluster_comparison_final_cleaned.R
# Author: Farhana Kabir
# Purpose: Compare 3-feature vs. 8-feature GMM clusters
# Outputs: ARI, NMI, Silhouette, Heatmap, Profile Plots
# ======================================================

# 1. Setup ----
library(tidyverse)
library(cluster)
library(factoextra)
library(mclust)
library(ggpubr)

# 2. Define output directory early ----
out_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/cluster_comparison"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 3. Load CSVs ----
df3 <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_3features.csv")
df8 <- read.csv("C:/Users/farha/ANLP_project/Copnew20/data/features/clustered_8features.csv")

# 4. Merge + Rename Columns ----
features_df <- inner_join(df3, df8, by = "uniform_id", suffix = c("_3F", "_8F")) %>%
  rename(Cluster_3F = Cluster_H3F)

# 5. Filter valid rows ----
features_common <- features_df %>% 
  filter(!is.na(Cluster_3F) & !is.na(Cluster_8F))
labels_3F <- features_common$Cluster_3F
labels_8F <- features_common$Cluster_8F

# 6. ARI & NMI ----
ari <- adjustedRandIndex(labels_8F, labels_3F)

calculate_nmi <- function(labels_true, labels_pred) {
  tab <- table(labels_true, labels_pred)
  MI <- sum(tab * log((tab * sum(tab)) / (rowSums(tab) %*% t(colSums(tab)) + 1e-10)))
  H_true <- -sum(rowSums(tab) / sum(tab) * log(rowSums(tab) / sum(tab) + 1e-10))
  H_pred <- -sum(colSums(tab) / sum(tab) * log(colSums(tab) / sum(tab) + 1e-10))
  MI / ((H_true + H_pred) / 2)
}
nmi <- calculate_nmi(labels_8F, labels_3F)

# 7. Silhouette Scores ----
features_3 <- features_common %>% select(prog_z_3F, reread_z_3F, lookback_z_3F)
features_8 <- features_common %>% select(prog_z_8F, reread_z_8F, lookback_z_8F, skip_8F, reg.in_8F, refix_8F, rate_8F, comprehension_score_8F)

sil_score_3 <- mean(silhouette(as.numeric(labels_3F), dist(scale(features_3)))[, 3])
sil_score_8 <- mean(silhouette(as.numeric(labels_8F), dist(scale(features_8)))[, 3])

# 8. Confusion Matrix Heatmap ----
conf_matrix <- table(Cluster_8F = labels_8F, Cluster_3F = labels_3F)
conf_df <- as.data.frame(conf_matrix)

heatmap_plot <- ggplot(conf_df, aes(x = factor(Cluster_8F), y = factor(Cluster_3F), fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "#2166AC", high = "#B2182B", name = "Count") +
  labs(title = "Cluster Assignment Comparison",
       x = "8-Feature Clusters", y = "3-Feature Clusters") +
  theme_minimal(base_size = 13)

# 9. Cluster Feature Profile Plot ----
profile_3F <- features_common %>%
  group_by(Cluster_3F) %>%
  summarise(across(c(prog_z_3F, reread_z_3F, lookback_z_3F), \(x) mean(x, na.rm = TRUE)))

profile_8F <- features_common %>%
  group_by(Cluster_8F) %>%
  summarise(across(c(prog_z_8F, reread_z_8F, lookback_z_8F), \(x) mean(x, na.rm = TRUE)))

profile_long <- bind_rows(
  profile_3F %>% 
    rename(Cluster = Cluster_3F, prog_z = prog_z_3F, reread_z = reread_z_3F, lookback_z = lookback_z_3F) %>% 
    mutate(Type = "3-Feature"),
  profile_8F %>% 
    rename(Cluster = Cluster_8F, prog_z = prog_z_8F, reread_z = reread_z_8F, lookback_z = lookback_z_8F) %>% 
    mutate(Type = "8-Feature")
) %>% 
  pivot_longer(cols = c("prog_z", "reread_z", "lookback_z"), names_to = "Feature", values_to = "Value")

profile_plot <- ggplot(profile_long, aes(x = Feature, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Cluster) +
  labs(title = "Cluster Feature Profiles",
       x = "Z-scored Feature", y = "Mean Z-score (unitless)") +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Set2")

# 10. ANOVA Tests ----
anova_results <- list()
for (feature in c("prog_z_8F", "reread_z_8F", "lookback_z_8F")) {
  formula <- as.formula(paste(feature, "~ factor(Cluster_8F)"))
  aov_result <- aov(formula, data = features_common)
  summary_result <- summary(aov_result)
  p_value <- summary_result[[1]][["Pr(>F)"]][1]
  
  anova_results[[feature]] <- tibble(
    Feature = feature,
    `F-statistic` = summary_result[[1]][["F value"]][1],
    `p-value` = p_value,
    Significance = ifelse(p_value < 0.001, "***",
                   ifelse(p_value < 0.01, "**",
                   ifelse(p_value < 0.05, "*", "ns")))
  )
}

anova_summary <- bind_rows(anova_results)
write.csv(anova_summary, file.path(out_dir, "anova_results.csv"), row.names = FALSE)

# 11. ANOVA Visualizations ----
plot_data <- features_common %>%
  select(Cluster_8F, prog_z_8F, reread_z_8F, lookback_z_8F) %>%
  pivot_longer(cols = -Cluster_8F, names_to = "Feature", values_to = "Zscore")

plot_data$Feature <- recode(plot_data$Feature,
                            "prog_z_8F" = "Progressive Fixation (Z-score)",
                            "reread_z_8F" = "Rereading Fixation (Z-score)",
                            "lookback_z_8F" = "Look-back Fixation (Z-score)")

anova_plot <- ggplot(plot_data, aes(x = factor(Cluster_8F), y = Zscore, fill = factor(Cluster_8F))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.4, color = "black") +
  facet_wrap(~ Feature, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(title = "ANOVA: Feature Distributions Across 8-Feature Clusters",
       x = "Cluster (8-Feature GMM)",
       y = "Z-scored Value (unitless)") +
  theme_minimal(base_size = 13)

ggsave(file.path(out_dir, "anova_feature_boxplots.png"), anova_plot, width = 10, height = 6)

# 12. Interpretation Table ----
interpretation <- tibble(
  Metric = c("Adjusted Rand Index", "Normalized Mutual Information", "Silhouette Score (8F)", "Silhouette Score (3F)"),
  Value = c(ari, nmi, sil_score_8, sil_score_3),
  Interpretation = c(
    ifelse(ari > 0.7, "Strong agreement", ifelse(ari > 0.4, "Moderate", "Weak")),
    ifelse(nmi > 0.7, "High mutual info", ifelse(nmi > 0.4, "Moderate", "Low")),
    ifelse(sil_score_8 > 0.5, "Well-separated", ifelse(sil_score_8 > 0.25, "Weakly separated", "Overlapping")),
    ifelse(sil_score_3 > 0.5, "Well-separated", ifelse(sil_score_3 > 0.25, "Weakly separated", "Overlapping"))
  )
)

# 13. Optional Permutation Test ----
perm_test <- function() {
  shuffled <- sample(labels_3F)
  adjustedRandIndex(labels_8F, shuffled)
}
null_dist <- replicate(1000, perm_test())
p_val <- mean(null_dist >= ari)

# 14. Save Final Outputs ----
write.csv(conf_df, file.path(out_dir, "confusion_matrix.csv"), row.names = FALSE)
write.csv(interpretation, file.path(out_dir, "metrics_summary.csv"), row.names = FALSE)
ggsave(file.path(out_dir, "heatmap_comparison.png"), heatmap_plot, width = 8, height = 6)
ggsave(file.path(out_dir, "cluster_profiles.png"), profile_plot, width = 10, height = 6)

# ✅ Final Message
message("✅ Final cluster comparison complete. ARI: ", round(ari, 3), ", NMI: ", round(nmi, 3), ", p = ", round(p_val, 4))
