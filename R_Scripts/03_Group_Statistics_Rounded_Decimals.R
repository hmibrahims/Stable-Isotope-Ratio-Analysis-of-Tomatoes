#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :Group Statistics (Rounded Decimal Digits)
#Date           :10.10.2025

# ------------------------------------------------------------
setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(dplyr)
library(readr)

in_csv  <- "Derived_From_Original_AtoJ_Exact_Decimals.csv"
out_csv <- "Derived_From_Original_AtoJ_rounded.csv"

y <- read_csv(in_csv, show_col_types = FALSE)

num_cols <- setdiff(names(y), c("product", "region", "farming_type", "n"))

y_round <- y %>%
  mutate(across(all_of(num_cols), ~ round(.x, 3)))

write_csv(y_round, out_csv)

