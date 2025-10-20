# Author Name   : Ibrahim Khalil
# Project Name  : Isotopic Analysis of Tomatoes
# Content Name  : Creating dataset and Table 3 — one-way ANOVA (Region × Farming Type)
#Date           : 17.10.2025
# ------------------------------------------------------------
# --- Rebuild replicates + One-way ANOVA (δ13C, δ15N) -> CSV ------------------
setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readxl)
library(dplyr)
library(readr)
library(tibble)

dir.create("tables", showWarnings = FALSE)

# 1) Read the lab sheet -------------------------------------------------------
raw <- read_excel("Original Data.xlsx")

# 2) Keep only replicate rows and standardize names ---------------------------
replicates <- raw %>%
  # keep rows with actual replicate measurements
  filter(!is.na(`d13CVPDB`) | !is.na(`d15NAIR`)) %>%
  transmute(
    identifier   = `Identifier 1`,
    product      = substr(`Identifier 1`, 1, 1),
    d13C_permil  = suppressWarnings(as.numeric(`d13CVPDB`)),
    d15N_permil  = suppressWarnings(as.numeric(`d15NAIR`))
  ) %>%
  # drop non-sample rows
  filter(!is.na(identifier), !is.na(d13C_permil) | !is.na(d15N_permil))

# 3) Map product -> region & farming_type (from your design) ------------------
replicates <- replicates %>%
  mutate(
    region = dplyr::case_when(
      product %in% c("A","C","D","E","F","G","H") ~ "Bavaria",
      product %in% c("B")                         ~ "Almeria",
      product %in% c("I","J")                     ~ "Souss-Massa",
      TRUE ~ NA_character_
    ),
    farming_type = dplyr::case_when(
      product %in% c("A","B","G","H")             ~ "Organic",
      product %in% c("C","D","E","F","I","J")     ~ "Conventional",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(region), !is.na(farming_type)) %>%
  mutate(
    region       = factor(region),
    farming_type = factor(farming_type)
  ) %>%
  droplevels()

# Optional: keep a permanent copy of the rebuilt replicates
write_csv(replicates, "tables/Rebuilt_Replicate_Data.csv")

# 4) Small helper to run one-way ANOVA and tidy output ------------------------
oneway_block <- function(data, response, factor_name, iso_label) {
  frm <- as.formula(paste(response, "~", factor_name))
  fit <- lm(frm, data = data)
  A   <- anova(fit)                         # Type I one-way = standard one-way ANOVA
  D   <- data.frame(Term = rownames(A), as.data.frame(A), check.names = FALSE)
  rownames(D) <- NULL
  
  get1 <- function(term, col)
    if (term %in% D$Term && col %in% names(D)) as.numeric(D[D$Term==term, col][1]) else NA_real_
  
  out <- tibble(
    isotope               = iso_label,
    factor                = ifelse(factor_name == "farming_type", "Farming type", "Region"),
    `Source`              = c(ifelse(factor_name=="farming_type","Farming type","Region"), "Residual"),
    df                    = c(get1(ifelse(factor_name=="farming_type","farming_type","region"), "Df"),
                              get1("Residuals","Df")),
    `Sum of squares`      = c(get1(ifelse(factor_name=="farming_type","farming_type","region"), "Sum Sq"),
                              get1("Residuals","Sum Sq")),
    F                     = c(get1(ifelse(factor_name=="farming_type","farming_type","region"), "F value"),
                              NA_real_),
    p                     = c(get1(ifelse(factor_name=="farming_type","farming_type","region"), "Pr(>F)"),
                              NA_real_)
  ) %>%
    mutate(
      `Sum of squares` = as.character(round(`Sum of squares`, 2)),
      F        = ifelse(is.na(F), "–", as.character(round(F, 2))),
      `p-value`= dplyr::case_when(
        is.na(p)      ~ "–",
        p < 0.001     ~ "<0.001 ***",
        p < 0.05      ~ paste0(round(p, 3), " *"),
        p > 0.1       ~ ">0.1",
        TRUE          ~ as.character(round(p, 3))
      )
    ) %>%
    select(isotope, `Source`, df, `Sum of squares`, F, `p-value`)
  out
}

# 5) Build all four one-way blocks -------------------------------------------
tab_13C_ft <- oneway_block(replicates, "d13C_permil", "farming_type", "δ¹³C")
tab_15N_ft <- oneway_block(replicates, "d15N_permil", "farming_type", "δ¹⁵N")
tab_13C_rg <- oneway_block(replicates, "d13C_permil", "region",        "δ¹³C")
tab_15N_rg <- oneway_block(replicates, "d15N_permil", "region",        "δ¹⁵N")

# 6) Combine and save (grouped the same way you presented) --------------------
oneway_table <- bind_rows(
  tab_13C_ft,
  tab_15N_ft,
  tab_13C_rg,
  tab_15N_rg
)

write_csv(oneway_table, "tables/OneWay_ANOVA_Table.csv")
print(oneway_table)
