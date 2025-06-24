# ================================================
# Script: 01a_preprocessing_and_zscoring.R
# Purpose: Extract base features (skip, reg.in, refix, rate) for MECO L2
# Author: Md Sabbir Hossain | Updated: 2025-06-20
# ================================================

# 1. Setup ----
rm(list = ls())
library(tidyverse)
library(psych)

# 2. Configuration ----
FIXATION_THRESHOLD <- 200     # Minimum number of fixations per participant
MAX_SKIP_RATE <- 0.4          # Skip rate threshold (exclude heavy skimmers)
MIN_ACCURACY <- 0.5           # Minimum comprehension accuracy

# 3. Load Eye Tracking Data (MECO L2) ----
load("C:/Users/farha/Downloads/osfstorage-archive (3)/release 2.0/version 2.0/wave 1/primary data/eye tracking data/joint_data_l2_trimmed_version2.0.rda")

# 4. Filter L2 Participants Only ----
joint.data <- joint.data %>% filter(lang != "en")
cat("✅ L2 participants retained:", length(unique(joint.data$uniform_id)), "\n")

# 5. Trim Outliers (Fixation duration and count) ----
initial_rows <- nrow(joint.data)
joint.data <- joint.data %>% filter(dur > 80 | is.na(dur))

# Per-participant 99th percentile cap for duration
crit <- joint.data %>%
  group_by(uniform_id) %>%
  summarise(dur.crit = quantile(dur, 0.99, na.rm = TRUE), .groups = "drop")
joint.data <- left_join(joint.data, crit, by = "uniform_id")
joint.data <- joint.data %>%
  filter(skip == 1 | dur < dur.crit)

# Trim by nfix (fixation count)
joint.data <- joint.data %>%
  filter(skip == 1 | nfix <= quantile(nfix, 0.99, na.rm = TRUE))
joint.data$nfix.inc <- ifelse(is.na(joint.data$nfix), 0, joint.data$nfix)

cat("✅ Trials retained after outlier trimming:", nrow(joint.data), "/", initial_rows, "\n")

# 6. Fixation Count Summary per Participant ----
fixation_check <- joint.data %>%
  group_by(uniform_id) %>%
  summarise(nfix_total = sum(nfix.inc, na.rm = TRUE), .groups = "drop")

# 7. Aggregate Participant-Level Gaze Features ----
# - skip: word skipping rate
# - reg.in: regression-in rate
# - refix: same-word refixation rate
by.sub <- joint.data %>%
  group_by(uniform_id) %>%
  summarise(
    lang = unique(lang),
    skip = mean(skip, na.rm = TRUE),
    reg.in = mean(reg.in, na.rm = TRUE),
    refix = mean(refix, na.rm = TRUE),
    .groups = "drop"
  )

# 8. Merge Reading Rate (WPM) ----
load("C:/Users/farha/Downloads/osfstorage-archive (3)/release 2.0/version 2.0/wave 1/primary data/eye tracking data/joint.l2.readrate_version2.0.rda")
readrate <- u %>%
  filter(rate < 1000) %>%
  group_by(uniform_id) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

by.sub <- left_join(by.sub, readrate, by = "uniform_id")

# 9. Merge Comprehension Accuracy ----
load("C:/Users/farha/Downloads/osfstorage-archive (3)/release 2.0/version 2.0/wave 1/primary data/comprehension data/joint.comp.l2.rda")
acc <- joint.comp %>%
  group_by(uniform_id) %>%
  summarise(acc = mean(accuracy, na.rm = TRUE), .groups = "drop")

by.sub <- left_join(by.sub, acc, by = "uniform_id")

# 10. Merge Fixation Totals ----
by.sub <- left_join(by.sub, fixation_check, by = "uniform_id")

# 11. Apply Participant-Level Filters ----
initial_n <- nrow(by.sub)
by.sub <- by.sub %>%
  filter(nfix_total > FIXATION_THRESHOLD,
         skip < MAX_SKIP_RATE,
         acc > MIN_ACCURACY)

cat("✅ Participants after filtering:", nrow(by.sub), " (removed", initial_n - nrow(by.sub), ")\n")

# 12. Z-Scoring of Features (for clustering, visualization) ----
safe_scale <- function(x) {
  if (all(is.na(x))) return(rep(NA, length(x)))
  if (sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}

by.sub <- by.sub %>%
  mutate(
    skip_z = safe_scale(skip),
    regin_z = safe_scale(reg.in),
    refix_z = safe_scale(refix),
    rate_z = safe_scale(rate)
  )

# 13. Save Cleaned Participant Features ----
write.csv(by.sub, "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1.csv", row.names = FALSE)
save(by.sub, file = "C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1.rda")

cat("\n✅ Final base features (raw + z-scored) saved.\n")
