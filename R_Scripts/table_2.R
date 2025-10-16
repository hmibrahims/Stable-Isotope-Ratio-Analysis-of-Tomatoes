# Author Name   : Ibrahim Khalil
# Project Name  : Isotopic Analysis of Tomatoes
# Content Name  : Table 2 — δ13C and δ15N mean ± SD by region & farming type
#Date           : 16.10.2025
# ------------------------------------------------------------


setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)

# ---- Read the original replicate dataset ----
data <- read_csv("Original_Replicates_Labeled.csv", show_col_types = FALSE)

# ---- Summarize by region and farming type ----
table2 <- data %>%
  group_by(region, farming_type) %>%
  summarise(
    n         = n(),
    d13C_mean = mean(d13C_permil, na.rm = TRUE),
    d13C_sd   = sd(d13C_permil,   na.rm = TRUE),
    d15N_mean = mean(d15N_permil, na.rm = TRUE),
    d15N_sd   = sd(d15N_permil,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(region, farming_type)

# ---- Create 'tables' folder
dir.create("tables", showWarnings = FALSE)

write_csv(table2, "tables/Table2_ProductType_Region_Isotopes.csv")
