# =============================================
# R Script: 01b_validation_checks.R (Final Updated)
# Purpose: Diagnostic checks for MECO L2 preprocessing
# Author: Farhana Kabir | Updated: 2025-06-21
# =============================================

# 1. Setup ----
rm(list = ls())
library(tidyverse)
library(moments)

# 2. Load Final Features ----
csv_final <- "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.csv"
by.sub_final <- read.csv(csv_final)

# 3. Basic Participant Stats ----
cat("\n===== PARTICIPANT STATS =====\n")
cat("Total participants:", nrow(by.sub_final), "\n")
cat("Unique IDs:", length(unique(by.sub_final$uniform_id)), "\n")
cat("Duplicated IDs:", sum(duplicated(by.sub_final$uniform_id)), "\n")

# 4. Missingness Report ----
cat("\n===== MISSINGNESS REPORT =====\n")
missing_counts <- sapply(by.sub_final, function(x) sum(is.na(x)))
print(missing_counts)

# 5. Skewness of Key Features ----
FEATURES <- c("skip", "reg.in", "refix", "rate",
              "prog_dur", "reread_dur", "lookback_dur",
              "prog_z", "reread_z", "lookback_z")

cat("\n===== FEATURE SKEWNESS (Moment-Based) =====\n")
skew <- sapply(by.sub_final[FEATURES], skewness, na.rm = TRUE)
print(round(skew, 2))

# Save skewness as CSV
plot_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
write.csv(round(skew, 2), file.path(plot_dir, "skewness_report.csv"))

# 6. Range Summary ----
cat("\n===== FEATURE RANGE SUMMARY =====\n")
feature_ranges <- t(sapply(by.sub_final[FEATURES], function(x) range(x, na.rm = TRUE)))
colnames(feature_ranges) <- c("Min", "Max")
print(round(feature_ranges, 2))
write.csv(round(feature_ranges, 2), file.path(plot_dir, "feature_ranges_report.csv"))

# 7. Comprehension Score Summary ----
cat("\n===== COMPREHENSION SCORE SUMMARY =====\n")
summary_stats <- summary(by.sub_final$comprehension_score)
print(round(summary_stats, 2))

# 8. Diagnostic Plots ----
# Histogram 1: Reading Rate
p1 <- ggplot(by.sub_final, aes(x = rate)) +
  geom_histogram(bins = 60, fill = "lightblue", color = "black") +
  labs(title = "Reading Rate Distribution", x = "Words per Minute (WPM)", y = "Participants") +
  theme_minimal()

# Histogram 2: Comprehension Score
p2 <- ggplot(by.sub_final, aes(x = comprehension_score)) +
  geom_histogram(bins = 60, fill = "salmon", color = "black") +
  labs(title = "Comprehension Score Distribution", x = "Composite Score", y = "Participants") +
  theme_minimal()

# Save plots
ggsave(file.path(plot_dir, "hist_rate_final.png"), plot = p1, width = 6, height = 4, dpi = 300)
cat("✅ Saved: hist_rate_final.png\n")

ggsave(file.path(plot_dir, "hist_comprehension_score_final.png"), plot = p2, width = 6, height = 4, dpi = 300)
cat("✅ Saved: hist_comprehension_score_final.png\n")

cat("\n✅ Final validation complete. All diagnostics and plots saved to:", plot_dir, "\n")
