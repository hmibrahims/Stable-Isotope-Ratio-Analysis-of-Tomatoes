#Author Name    :Ibrahim Khalil
#Project Name   :Isotopic Analysis of Tomatoes
#Content Name   :PCA biplot (scores + loading vectors)
#Date           :15.10.2025
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

loadings <- as.data.frame(pc$rotation[, 1:2])
loadings$var <- rownames(loadings)

rng <- max(abs(scores$PC1), abs(scores$PC2))
L <- max(sqrt(loadings$PC1^2 + loadings$PC2^2))
scale_factor <- 0.8 * rng / L
loadings_scaled <- transform(loadings,
                             xend = PC1 * scale_factor,
                             yend = PC2 * scale_factor
)

pal_region <- c(
  "Almeria"     = "#1b9e77",
  "Bavaria"     = "#d95f02",
  "Souss-Massa" = "#7570b3"
)
shape_ft <- c("Conventional" = 17, "Organic" = 16)

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_point(data = scores, aes(PC1, PC2, color = region, shape = farming_type), size = 3) +
  geom_segment(data = loadings_scaled,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.22, "cm")), linewidth = 0.7) +
  geom_text(data = loadings_scaled,
            aes(x = xend, y = yend, label = var),
            nudge_x = 0.05, nudge_y = 0.05, size = 4) +
  scale_color_manual(values = pal_region) +
  scale_shape_manual(values = shape_ft) +
  labs(x = lab_x, y = lab_y) +
  theme_classic(base_size = 14) +
  theme(legend.position = "right")

ggsave("figures/Figure5_PCA_biplot.tiff", p, device = "tiff", dpi = 300, width = 13.7, height = 9.7, units = "in")
