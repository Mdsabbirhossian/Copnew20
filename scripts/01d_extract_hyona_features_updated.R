# ================================================================
# Script: 01d_extract_hyona_features.R (Final Version)
# Purpose: Extract 10 Hyönä-style gaze features from MECO-L2 Wave 1
# Author: Farhana Kabir | Updated: 2025-06-21
# ================================================================

# 1. Setup ----
rm(list = ls())
library(tidyverse)

# 2. Path Configuration ----
eye_data_path <- "C:/Users/farha/Downloads/osfstorage-archive (3)/release 2.0/version 2.0/wave 1/primary data/eye tracking data/joint_data_l2_trimmed_version2.0.rda"
feat_input    <- "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1.rda"
output_rda    <- "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.rda"
output_csv    <- "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.csv"
diagnostic_log <- "C:/Users/farha/ANLP_project/Copnew20/data/features/diagnostics_log.txt"

# 3. Load Data ----
load(eye_data_path)  # loads joint.data
load(feat_input)     # loads by.sub

# 4. Column Check ----
required_cols <- c("trialnum", "wordnum", "sentnum", "dur", "reread", "firstrun.dur", "lang")
missing <- setdiff(required_cols, colnames(joint.data))
if (length(missing) > 0) stop("❌ Missing required columns: ", paste(missing, collapse = ", "))

cat("✅ joint.data loaded. Columns: ", paste(colnames(joint.data), collapse = ", "), "\n")

# 5. Filter Non-English L2 Participants ----
joint.data <- joint.data %>% filter(lang != "en")

# 6. Compute Sentence-Based Lookback Indicator ----
joint.data <- joint.data %>%
  filter(!is.na(wordnum), !is.na(sentnum)) %>%
  group_by(uniform_id, trialnum) %>%
  mutate(fixation_order = row_number()) %>%
  arrange(uniform_id, trialnum, fixation_order) %>%
  mutate(
    prev_sent = lag(sentnum),
    prev_word = lag(wordnum),
    lookback = case_when(
      !is.na(prev_sent) & sentnum < prev_sent ~ 1,
      !is.na(prev_sent) & sentnum == prev_sent & !is.na(prev_word) & wordnum < prev_word ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# 7. Lookback Sanity Check ----
lookback_check <- joint.data %>%
  group_by(uniform_id) %>%
  summarise(lookback_ratio = mean(lookback, na.rm = TRUE), .groups = "drop")

# 8. Compute Fixation Duration Features ----
hyona_features <- joint.data %>%
  group_by(uniform_id) %>%
  summarise(
    prog_dur     = mean(firstrun.dur, na.rm = TRUE),
    reread_dur   = mean(dur[reread == 1 & !is.na(dur)], na.rm = TRUE),
    lookback_dur = mean(dur[lookback == 1 & !is.na(dur)], na.rm = TRUE),
    reread_count = sum(reread == 1, na.rm = TRUE),
    lookback_count = sum(lookback == 1, na.rm = TRUE),
    .groups = "drop"
  )

# 9. Z-score Standardization ----
safe_scale <- function(x) {
  if (all(is.na(x))) return(rep(NA, length(x)))
  if (sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}

hyona_features <- hyona_features %>%
  mutate(
    prog_z = safe_scale(prog_dur),
    reread_z = safe_scale(reread_dur),
    lookback_z = safe_scale(lookback_dur),
    comprehension_score = 0.4 * prog_z + 0.3 * reread_z + 0.3 * lookback_z,
    lookback_proxy = lookback_z + safe_scale(lookback_count)
  )

# 10. Merge with Participant-Level Features ----
by.sub <- by.sub %>% left_join(hyona_features, by = "uniform_id")

# 11. Select Final Features ----
selected <- by.sub %>% select(
  uniform_id,
  prog_dur, reread_dur, lookback_dur,
  prog_z, reread_z, lookback_z,
  skip, reg.in, refix, rate,
  comprehension_score
)

# 12. Diagnostics Output ----
sink(diagnostic_log)  # Start writing to diagnostics_log.txt
cat("=== Final Feature Set Summary ===\n")
cat("Participants:", nrow(selected), "\n")
cat("Complete cases (%):", round(mean(complete.cases(selected)) * 100, 1), "%\n")
cat("Lookback participants (%):", round(mean(!is.na(selected$lookback_dur)) * 100, 1), "%\n")
cat("Correlation (lookback_count ~ reread_count):", round(cor(hyona_features$lookback_count, hyona_features$reread_count, use = "complete.obs"), 3), "\n")
cat("Mean rate (WPM):", round(mean(selected$rate, na.rm = TRUE), 2), "\n")
cat("Progressive duration range (ms):", round(range(hyona_features$prog_dur, na.rm = TRUE), 2), "\n")
cat("✅ Diagnostics completed.\n")
sink()  # End logging

# 12a. Distribution and Correlation Visualizations ----
library(ggplot2)
library(corrplot)

# Prepare output directory
plot_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Convert to dataframe
df <- selected

# Distribution: prog_z
p1 <- ggplot(df, aes(x = prog_z)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.1) +
  theme_minimal() +
  labs(title = "Distribution of Progressive Fixation (prog_z)", x = "prog_z", y = "Density")
ggsave(file.path(plot_dir, "distribution_prog_z.png"), p1, width = 6, height = 4, dpi = 300)

# Distribution: rate
p2 <- ggplot(df, aes(x = rate)) +
  geom_histogram(bins = 30, fill = "seagreen", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1.1) +
  theme_minimal() +
  labs(title = "Distribution of Reading Rate (WPM)", x = "rate", y = "Density")
ggsave(file.path(plot_dir, "distribution_rate.png"), p2, width = 6, height = 4, dpi = 300)

# Correlation heatmap
# Square Correlation heatmap
numeric_df <- df %>% select(-uniform_id) %>% drop_na()
corr_matrix <- cor(numeric_df, use = "complete.obs")
png(file.path(plot_dir, "correlation_matrix_square_10features.png"), width = 800, height = 800)
corrplot(corr_matrix, method = "color", type = "full", order = "hclust",
         tl.cex = 0.9, addCoef.col = "black", number.cex = 0.8,
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()


# 13. Save Final Output ----
save(selected, file = output_rda)
write.csv(selected, output_csv, row.names = FALSE)
cat("\n✅ Final 10 features saved to:\n", output_csv, "\n")
