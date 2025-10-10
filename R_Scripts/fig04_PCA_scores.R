#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :PCA scores plot (PC1 vs PC2)
#Date           :10.10.2025
# ------------------------------------------------------------

setwd("C:/Users/User/OneDrive/Desktop/Stable-Isotope-Ratio-Analysis-of-Tomatoes")

library(readr)
library(dplyr)
library(ggplot2)

dat <- read_csv("Original_Replicates_Labeled.csv", show_col_types = FALSE)

X <- dat %>%
  select(d13C_permil, d15N_permil, wtN_percent, wtC_percent, CN_molar) %>%
  as.data.frame()

pc <- prcomp(X, center = TRUE, scale. = TRUE)

ve <- 100 * pc$sdev^2 / sum(pc$sdev^2)
lab_x <- paste0("PC1 (", round(ve[1], 1), "%)")
lab_y <- paste0("PC2 (", round(ve[2], 1), "%)")

scores <- as.data.frame(pc$x) %>%
  select(PC1, PC2) %>%
  bind_cols(dat %>% select(region, farming_type))

pal_region <- c(
  "Almeria"     = "#1b9e77",
  "Bavaria"     = "#d95f02",
  "Souss-Massa" = "#7570b3"
)
shape_ft <- c("Conventional" = 17, "Organic" = 16)

p <- ggplot(scores, aes(PC1, PC2, color = region, shape = farming_type)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_point(size = 3) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = lab_x, y = lab_y) +
  theme_classic(base_size = 14) +
  theme(legend.position = "right")

ggsave("Figure4_PCA_scores.tiff", p, device = "tiff", dpi = 300, width = 6.6, height = 6.6, units = "in")

