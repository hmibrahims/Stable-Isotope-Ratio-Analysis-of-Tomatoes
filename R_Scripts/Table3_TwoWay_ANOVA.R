# Author Name   : Ibrahim Khalil
# Project Name  : Isotopic Analysis of Tomatoes
# Content Name  : Table 3 — Two-way ANOVA (Region × Farming Type)
#Date           : 16.10.2025
# ------------------------------------------------------------

setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)
library(broom)

# 1) Read data
dat <- read_csv("Original_Replicates_Labeled.csv", show_col_types = FALSE) %>%
  mutate(
    region       = factor(region),
    farming_type = factor(farming_type)
  )

# 2) Helper to run ANOVA and tidy it (no forcing)
do_anova <- function(response, iso_label) {
  fit <- aov(as.formula(paste(response, "~ region * farming_type")), data = dat)
  tidy(fit) |>
    select(term, df, sumsq, statistic, p.value) |>
    mutate(
      isotope = iso_label,
      `Source of variation` = recode(term,
                                     "region"               = "Region",
                                     "farming_type"         = "Farming type",
                                     "region:farming_type"  = "Region × Farming type",
                                     "Residuals"            = "Residual"
      )
    ) |>
    # keep only the usual rows that exist in the model output
    filter(`Source of variation` %in% c("Region","Farming type","Region × Farming type","Residual")) |>
    arrange(factor(`Source of variation`,
                   levels = c("Region","Farming type","Region × Farming type","Residual"))) |>
    transmute(
      isotope,
      `Source of variation`,
      df,
      `Sum of squares` = round(sumsq, 3),
      F                = round(statistic, 3),
      `p-value`        = round(p.value, 3)
    )
}

# 3) Run for both isotopes
tab13C <- do_anova("d13C_permil", "δ13C")
tab15N <- do_anova("d15N_permil", "δ15N")

# 4) Combine and save
table3 <- bind_rows(tab13C, tab15N)

dir.create("tables", showWarnings = FALSE)
write_csv(table3, "tables/Table3_TwoWay_ANOVA.csv")
