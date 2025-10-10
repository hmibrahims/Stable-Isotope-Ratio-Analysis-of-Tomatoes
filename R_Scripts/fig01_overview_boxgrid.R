#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :Overview of traits (2×2 boxplot grid)
#Date           :10.10.2025
# ------------------------------------------------------------

setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(42)

prod <- read_csv("Derived_From_Original_AtoJ_Exact_Decimals.csv", show_col_types = FALSE)

# Expect these columns: product, region, farming_type, d13C_mean, d13C_sd, d15N_mean, d15N_sd,
# wtN_mean, wtN_sd, wtC_mean, wtC_sd, CN_molar_mean, CN_molar_sd
prod_long <- prod %>%
  transmute(
    product, region, farming_type,
    `δ13C (‰)` = d13C_mean,
    `δ15N (‰)` = d15N_mean,
    `wt%N (%)` = wtN_mean,
    `C:N (molar)` = CN_molar_mean
  ) %>%
  pivot_longer(cols = c(`δ13C (‰)`,`δ15N (‰)`,`wt%N (%)`,`C:N (molar)`),
               names_to = "variable", values_to = "value")

pal_region <- c("Almeria"="#1b9e77","Bavaria"="#d95f02","Souss-Massa"="#7570b3")
shape_ft   <- c("Conventional"=17,"Organic"=16)

p <- ggplot(prod_long, aes(x = farming_type, y = value)) +
  geom_boxplot(fill = NA, colour = "black", linewidth = 0.4, width = 0.6, outlier.shape = NA) +
  geom_point(aes(color = region, shape = farming_type),
             position = position_jitter(width = 0.18, height = 0), size = 2.1) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  scale_color_manual(values = pal_region, name = "Region") +
  scale_shape_manual(values = shape_ft, name = "Farming type") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.4),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    plot.margin = margin(6,6,6,6)
  )

ggsave("figures/Figure1_overview_traits.tiff", p,
       width = 174, height = 122, units = "mm",
       dpi = 600, device = "tiff", compression = "lzw", bg = "white")

