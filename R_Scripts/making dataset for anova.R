# Author Name   : Ibrahim Khalil
# Project Name  : Isotopic Analysis of Tomatoes
# Content Name  : Creating dataset and Table 3 — Two-way ANOVA (Region × Farming Type)
#Date           : 17.10.2025
# ------------------------------------------------------------
# === Rebuild the replicate-level dataset used in  ANOVA ===
setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readxl)
library(dplyr)
library(readr)

raw <- read_excel("Original Data.xlsx")

replicates <- raw %>%
  filter(!is.na(`d13CVPDB`), !is.na(`d15NAIR`)) %>%        # keep real samples only
  transmute(
    identifier   = `Identifier 1`,
    product      = substr(identifier, 1, 1),
    d13C_permil  = as.numeric(`d13CVPDB`),
    d15N_permil  = as.numeric(`d15NAIR`)
  ) %>%
  mutate(
    region = case_when(
      product %in% c("A","C","D","E","F","G","H") ~ "Bavaria",
      product %in% c("B") ~ "Almeria",
      product %in% c("I","J") ~ "Souss-Massa"
    ),
    farming_type = case_when(
      product %in% c("A","B","G","H") ~ "Organic",
      product %in% c("C","D","E","F","I","J") ~ "Conventional"
    )
  ) %>%
  filter(!is.na(region), !is.na(farming_type)) %>%
  mutate(across(c(region,farming_type), as.factor))

# Save a permanent copy
write_csv(replicates, "tables/Rebuilt_Replicate_Data.csv")

# Quick check
nrow(replicates)          # should show ≈52–53
table(replicates$region, replicates$farming_type)



library(car)
options(contrasts = c("contr.sum","contr.poly"))

fit13 <- lm(d13C_permil ~ region * farming_type, data = replicates)
fit15 <- lm(d15N_permil ~ region * farming_type, data = replicates)

# Type I (sequential) ANOVA — matches your original table numerically
anova(fit13)
anova(fit15)


table3 <- broom::tidy(anova(fit13)) %>%
  mutate(isotope = "δ13C") %>%
  bind_rows(
    broom::tidy(anova(fit15)) %>% mutate(isotope = "δ15N")
  )

write_csv(table3, "tables/Table3_TwoWay_ANOVA.csv")

