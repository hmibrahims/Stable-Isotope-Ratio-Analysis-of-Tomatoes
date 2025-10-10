#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :Isotopes by farming type (stacked panels)
#Date           :10.10.2025
# ------------------------------------------------------------

setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

set.seed(42)

dat <- read_csv("Original_Replicates_Labeled.csv", show_col_types = FALSE)

pal_region <- c("Almeria"="#1b9e77","Bavaria"="#d95f02","Souss-Massa"="#7570b3")
shape_ft   <- c("Conventional"=17,"Organic"=16)

p1 <- ggplot(dat, aes(farming_type, d13C_permil)) +
  geom_boxplot(fill = NA, colour = "black", linewidth = 0.4, width = 0.6, outlier.shape = NA) +
  geom_point(aes(color = region, shape = farming_type),
             position = position_jitter(width = 0.18, height = 0), size = 2.0) +
  scale_color_manual(values = pal_region, name = "Region") +
  scale_shape_manual(values = shape_ft, name = "Farming type") +
  labs(x = NULL, y = expression(delta^13*C~("\u2030"))) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

p2 <- ggplot(dat, aes(farming_type, d15N_permil)) +
  geom_boxplot(fill = NA, colour = "black", linewidth = 0.4, width = 0.6, outlier.shape = NA) +
  geom_point(aes(color = region, shape = farming_type),
             position = position_jitter(width = 0.18, height = 0), size = 2.0) +
  scale_color_manual(values = pal_region, name = "Region") +
  scale_shape_manual(values = shape_ft, name = "Farming type") +
  labs(x = NULL, y = expression(delta^15*N~("\u2030"))) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

fig <- p1 / p2
ggsave("figures/Figure2_isotopes_by_farmingtype.tiff", fig,
       width = 84, height = 130, units = "mm",
       dpi = 600, device = "tiff", compression = "lzw", bg = "white")

