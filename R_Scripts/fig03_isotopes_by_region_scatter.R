#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :δ¹⁵N vs δ¹³C scatter by region
#Date           :10.10.2025
# ------------------------------------------------------------

setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)
library(ggplot2)

dat <- read_csv("Original_Replicates_Labeled.csv", show_col_types = FALSE)

pal_region <- c(
  "Almeria"     = "#1b9e77",
  "Bavaria"     = "#d95f02",
  "Souss-Massa" = "#7570b3"
)
shape_ft <- c("Conventional" = 17, "Organic" = 16)

p <- ggplot(dat, aes(d13C_permil, d15N_permil, color = region, shape = farming_type)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = expression(delta^13*C~("\u2030")), y = expression(delta^15*N~("\u2030"))) +
  theme_classic(base_size = 14) +
  theme(legend.position = "right")

ggsave("Figure3_isotopes_by_region_scatter.tiff", p, device = "tiff", dpi = 300, width = 6.6, height = 6.6, units = "in")
