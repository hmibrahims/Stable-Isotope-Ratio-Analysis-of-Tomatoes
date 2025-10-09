#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :Group Statistics (Exact Decimal Digits)
#Date           :10.10.2025

# ------------------------------------------------------------
setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(dplyr)
library(readr)

in_csv  <- "Original_Replicates_Labeled.csv"
out_csv <- "Derived_From_Original_AtoJ_Exact_Decimals.csv"

x <- read_csv(in_csv, show_col_types = FALSE)

summ <- x %>%
  group_by(product, region, farming_type) %>%
  summarise(
    n             = dplyr::n(),
    d13C_mean     = mean(d13C_permil, na.rm = TRUE),
    d13C_sd       = sd(d13C_permil,   na.rm = TRUE),
    d15N_mean     = mean(d15N_permil, na.rm = TRUE),
    d15N_sd       = sd(d15N_permil,   na.rm = TRUE),
    wtN_mean      = mean(wtN_percent, na.rm = TRUE),
    wtN_sd        = sd(wtN_percent,   na.rm = TRUE),
    wtC_mean      = mean(wtC_percent, na.rm = TRUE),
    wtC_sd        = sd(wtC_percent,   na.rm = TRUE),
    CN_molar_mean = mean(CN_molar,    na.rm = TRUE),
    CN_molar_sd   = sd(CN_molar,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(product)

write_csv(summ, out_csv)

