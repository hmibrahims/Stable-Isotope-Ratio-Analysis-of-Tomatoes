#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :Label Replicates
#Date           :10.10.2025

# ------------------------------------------------------------
#Set working directory
setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

#packages
library(readxl)
library(dplyr)
library(stringr)
library(readr)

#Input / Output files
in_xlsx  <- "Original Data.xlsx"
out_csv  <- "Original_Replicates_Labeled.csv"
sheet_nm <- "Data for report"

#Read and clean data
raw <- read_excel(in_xlsx, sheet = sheet_nm)

dat <- raw %>%
  filter(!is.na(`Identifier 1`)) %>%
  filter(`Identifier 1` != "Additional replicate analyses") %>%
  mutate(
    identifier = `Identifier 1`,
    product    = str_extract(identifier, "^[A-Z]")
  )

#Map products to region and farming type
product_map <- tribble(
  ~product, ~region,      ~farming_type,
  "A", "Bavaria",    "Organic",
  "B", "Almeria",    "Organic",
  "C", "Bavaria",    "Conventional",
  "D", "Bavaria",    "Conventional",
  "E", "Bavaria",    "Conventional",
  "F", "Bavaria",    "Conventional",
  "G", "Bavaria",    "Organic",
  "H", "Bavaria",    "Organic",
  "I", "Bavaria",    "Organic",
  "J", "Souss-Massa","Conventional"
)

dat <- dat %>%
  left_join(product_map, by = "product") %>%
  transmute(
    identifier,
    product,
    region,
    farming_type,
    d13C_permil = `d13CVPDB`,
    d15N_permil = `d15NAIR`,
    wtN_percent = `wt% N`,
    wtC_percent = `wt% C`
  )

#Calculate C/N molar ratio
C_atomic <- 12.011
N_atomic <- 14.007
dat <- dat %>%
  mutate(CN_molar = (wtC_percent / C_atomic) / (wtN_percent / N_atomic))

#Save CSV
write_csv(dat, out_csv)
