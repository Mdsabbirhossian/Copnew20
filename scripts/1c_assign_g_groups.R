# ========================================
# R Script: 01c_assign_g_groups.R (Final, Updated)
# Purpose: Assign G1–G3 using Hyönä-style logic + fallback + light imputation
# Author: Farhana Kabir | Date: 2025-06-21
# ========================================

# 1. Setup ----
rm(list = ls())
library(tidyverse)

# 2. Load Final Feature File ----
load("C:/Users/farha/ANLP_project/Copnew20/data/features/by_sub_features_wave1_final_10features.rda")
by.sub_final <- selected  # assuming object is called 'selected'

# 3. Define z-score columns used for assignment
z_cols <- c("prog_z", "lookback_z", "reread_z")
stopifnot(all(z_cols %in% colnames(by.sub_final)))

# 4. Light Imputation for NA values (median-based)
for (col in z_cols) {
  med_val <- median(by.sub_final[[col]], na.rm = TRUE)
  by.sub_final[[col]][is.na(by.sub_final[[col]])] <- med_val
}

# 5. Primary Assignment (Hyönä-style rules)
by.sub_final$G_group <- with(by.sub_final, ifelse(
  prog_z < -0.5 & lookback_z < -0.3 & reread_z < -0.2, "G1",        # Fast / Efficient
  ifelse(
    prog_z > 0.2 & lookback_z < 0.3 & reread_z > 0, "G2",           # Careful / Balanced
    ifelse(
      lookback_z > 0.5 & reread_z > 0.5, "G3",                      # Risky / Regressive
      NA
    )
  )
))

# 6. Fallback Assignment for Remaining (Distance-Based)
assign_remaining <- function(df) {
  for (i in which(is.na(df$G_group))) {
    row <- df[i, z_cols]
    dist_G1 <- sqrt((row$prog_z + 0.5)^2 + (row$lookback_z + 0.3)^2 + (row$reread_z + 0.2)^2)
    dist_G2 <- sqrt((row$prog_z - 0.3)^2 + (row$lookback_z)^2 + (row$reread_z - 0.2)^2)
    dist_G3 <- sqrt((row$prog_z)^2 + (row$lookback_z - 0.7)^2 + (row$reread_z - 0.7)^2)
    df$G_group[i] <- c("G1", "G2", "G3")[which.min(c(dist_G1, dist_G2, dist_G3))]
  }
  return(df)
}
by.sub_final <- assign_remaining(by.sub_final)

# 7. Summary Statistics ----
cat("\n===== Final G1–G3 Group Distribution =====\n")
print(table(by.sub_final$G_group))

# 8. Save Data ----
output_dir <- "C:/Users/farha/ANLP_project/Copnew20/data/features"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

save(by.sub_final, file = file.path(output_dir, "by_sub_with_ggroups.rda"))
write.csv(by.sub_final, file.path(output_dir, "by_sub_with_ggroups.csv"), row.names = FALSE)

# 9. Visualization: G-Group Assignment ----
plot_dir <- "C:/Users/farha/ANLP_project/Copnew20/outputs/plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

g_colors <- c("G1" = "skyblue", "G2" = "seagreen3", "G3" = "tomato")

gg_bar <- ggplot(by.sub_final, aes(x = G_group, fill = G_group)) +
  geom_bar(width = 0.6) +
  scale_fill_manual(values = g_colors) +
  labs(title = "Distribution of G1–G3 Reader Groups (All Assigned)",
       x = "Reader Group", y = "Number of Participants") +
  theme_minimal()

ggsave(file.path(plot_dir, "bar_g_group_all_assigned.png"), gg_bar, width = 5, height = 4, dpi = 300)

cat("\n✅ All participants successfully grouped into G1–G3. Plot saved.\n")
