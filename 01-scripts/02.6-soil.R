library(tidyverse)

source("01-scripts/01.1d-soil.R")

############
# pH -------
############

# ggplot(df.soil.ph, aes(x = pH, y = Environment, fill = Environment)) +
#   geom_boxplot(alpha = .3, width = .2) +
#   geom_jitter(aes(colour = Environment), position = "dodge") +
#   theme_light()

############
# Texture ----
############

# library(ggtern)
# ggtern(
#   data = df.soil.texture,
#   aes(
#     x = prop_sand,
#     y = prop_silt,
#     z = prop_clay,
#     colour = Environment
#   )
# ) +
#   geom_point(size = 3, alpha = 0.8) +
#   labs(
#     T = "Clay (%)",
#     L = "Sand (%)",
#     R = "Silt (%)"
#   ) +
#   theme_bw()

# ## Pending: warning message if this do not sum 100 in all samples
# df.soil.texture$prop_silt + df.soil.texture$prop_clay + df.soil.texture$prop_sand

###############
# Nutrients ----
###############

nutrient_vars <- c("Total_MO", "N", "Phosphorus", "Potassium")

for (variable in nutrient_vars) {
  p <- ggplot(
    df.soil.nutrients,
    aes(x = .data[[variable]], y = microsite)
  ) +
    geom_boxplot(width = .2, outliers = FALSE) +
    geom_jitter(height = .1, alpha = .3) +
    facet_wrap(~Environment, ncol = 1) +
    labs(x = variable)

  print(p)
}

########################
# PCA of soil nutrients ----
########################

df.soil.pca <- df.soil.nutrients |>
  select(
    Individual,
    all_of(nutrient_vars)
  )

pca.soil <- df.soil.pca |>
  select(-Individual) |>
  prcomp(
    center = TRUE,
    scale. = TRUE
  )

print(summary(pca.soil))

loadings.soil <- pca.soil$rotation |>
  as.data.frame() |>
  rownames_to_column("Variable")

print(loadings.soil)

scores.soil <- as.data.frame(pca.soil$x) |>
  mutate(
    Individual = df.soil.pca$Individual
  ) |>
  left_join(
    df.soil.nutrients |>
      select(Individual, Environment, microsite),
    by = "Individual"
  )

pca.scores <- ggplot(
  scores.soil,
  aes(x = PC1, y = PC2, colour = microsite, shape = Environment)
) +
  geom_point(size = 3, alpha = 0.8) +
  theme_classic() +
  labs(
    x = paste0(
      "PC1 (",
      round(summary(pca.soil)$importance[2, 1] * 100, 1),
      "%) ",
      "General gradient of fertility and soil enrichment"
    ),
    y = paste0(
      "PC2 (",
      round(summary(pca.soil)$importance[2, 2] * 100, 1),
      "%) ",
      "Less potassium and phosphorus"
    ),
    colour = "Microsite",
    shape = "Canopy"
  )

print(pca.scores)

p.pc2 <- ggplot(scores.soil, aes(y = microsite, x = PC2)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  geom_jitter(height = .2)

print(p.pc2)

p.potassium <- ggplot(
  df.soil.nutrients,
  aes(x = Potassium, y = microsite)
) +
  geom_violin() +
  geom_boxplot(width = .2) +
  geom_jitter(height = .2)

print(p.potassium)

library(factoextra)

p.biplot <- fviz_pca_biplot(
  pca.soil,
  geom.ind = "point",
  habillage = scores.soil$Environment,
  addEllipses = FALSE,
  label = "var",
  repel = TRUE
) +
  theme_classic()

print(p.biplot)

cat("Soil exploratory analysis completed.\n")
