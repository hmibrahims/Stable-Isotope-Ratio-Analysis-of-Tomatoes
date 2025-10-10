#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :Overview of traits (2×2 boxplot grid)
#Date           :10.10.2025
# ------------------------------------------------------------

setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

dat <- read_csv("Original_Replicates_Labeled.csv", show_col_types = FALSE)

pal_region <- c(
  "Almeria"     = "#1b9e77",
  "Bavaria"     = "#d95f02",
  "Souss-Massa" = "#7570b3"
)
shape_ft <- c("Conventional" = 17, "Organic" = 16)

p1 <- ggplot(dat, aes(farming_type, d13C_permil, color = region, shape = farming_type)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), size = 2.2) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = NULL, y = expression(delta^13*C~("\u2030"))) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

p2 <- ggplot(dat, aes(farming_type, d15N_permil, color = region, shape = farming_type)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), size = 2.2) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = NULL, y = expression(delta^15*N~("\u2030"))) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

p3 <- ggplot(dat, aes(farming_type, wtN_percent, color = region, shape = farming_type)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), size = 2.2) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = NULL, y = "wt%N") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

p4 <- ggplot(dat, aes(farming_type, CN_molar, color = region, shape = farming_type)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), size = 2.2) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = NULL, y = "C:N (molar)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

legend_plot <- ggplot(dat, aes(farming_type, d13C_permil, color = region, shape = farming_type)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  guides(color = guide_legend(title = "Region"), shape = guide_legend(title = "Farming type")) +
  theme_void(base_size = 12) +
  theme(legend.position = "right", legend.box = "vertical")

grid <- (p1 | p2) / (p3 | p4)
final <- grid + plot_layout(guides = "collect") & theme(legend.position = "none")
wrapped <- final | legend_plot + plot_layout(widths = c(1, 0.28))

ggsave("Figure1_overview_traits.tiff", wrapped, device = "tiff", dpi = 300, width = 13.7, height = 9.6, units = "in")
