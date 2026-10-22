library(tidyverse)
library(patchwork)

source("01-scripts/01.2a-survival_growth.R")

df.growth.volume.eda <- df.growth.volume.diff |>
  filter(
    as.character(quercus_sp) != "",
    !is.na(Environment),
    !is.na(microsite)
  ) |>
  mutate(
    growth_volume_rel = growth_volume / t1,
    log_ratio_volume = if_else(
      t1 > 0 & t2 > 0,
      log(t2 / t1),
      NA_real_
    ),
    negative_growth = growth_volume < 0,
    quercus_sp = droplevels(quercus_sp),
    Environment = droplevels(Environment),
    microsite = droplevels(microsite)
  )

df.growth.twig.eda <- df.growth |>
  filter(as.character(quercus_sp) != "") |>
  select(
    id_quercus, id_twig, Individual, quercus_sp, Environment,
    growth_stage, height, diam
  ) |>
  pivot_wider(
    names_from = growth_stage,
    values_from = c(height, diam)
  ) |>
  mutate(
    height_growth = height_t2 - height_t1,
    diam_growth = diam_t2 - diam_t1,
    microsite = case_when(
      substring(as.character(Individual), 1, 1) == "A" ~ "Genista scorpius",
      substring(as.character(Individual), 1, 1) == "C" ~ "open",
      substring(as.character(Individual), 1, 1) == "J" ~ "Cistus ladanifer",
      substring(as.character(Individual), 1, 1) == "R" ~ "Rosa canina",
      TRUE ~ NA_character_
    ),
    microsite = factor(
      microsite,
      levels = c("open", "Rosa canina", "Cistus ladanifer", "Genista scorpius")
    )
  ) |>
  filter(
    !is.na(height_growth),
    !is.na(diam_growth),
    !is.na(microsite)
  ) |>
  mutate(
    quercus_sp = droplevels(quercus_sp),
    Environment = droplevels(Environment)
  )

summarise_growth <- function(data, variable) {
  data |>
    group_by(quercus_sp, Environment, microsite) |>
    summarise(
      n = sum(!is.na(.data[[variable]])),
      n_missing = sum(is.na(.data[[variable]])),
      min = min(.data[[variable]], na.rm = TRUE),
      q25 = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      median = median(.data[[variable]], na.rm = TRUE),
      mean = mean(.data[[variable]], na.rm = TRUE),
      q75 = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      max = max(.data[[variable]], na.rm = TRUE),
      sd = sd(.data[[variable]], na.rm = TRUE),
      n_negative = sum(.data[[variable]] < 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(variable = variable)
}

summ.quercus.growth <- bind_rows(
  summarise_growth(df.growth.volume.eda, "growth_volume"),
  summarise_growth(df.growth.volume.eda, "growth_volume_rel"),
  summarise_growth(df.growth.volume.eda, "log_ratio_volume"),
  summarise_growth(df.growth.twig.eda, "height_growth"),
  summarise_growth(df.growth.twig.eda, "diam_growth")
) |>
  relocate(variable)

write.csv2(
  summ.quercus.growth,
  file = "00-data/processed/summ.quercus_growth.csv",
  row.names = FALSE
)

plot_growth <- function(data, x, y, ylab, title) {
  ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]])) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
    geom_violin(alpha = 0.25, colour = NA, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.12, alpha = 0.22, size = 1) +
    labs(x = NULL, y = ylab, title = title) +
    theme_light() +
    theme(legend.position = "none")
}

p_volume_species <- plot_growth(
  df.growth.volume.eda, "quercus_sp", "growth_volume",
  "Volume growth (t2 - t1, cm3)",
  "Volume growth by species"
)

p_volume_environment <- plot_growth(
  df.growth.volume.eda, "Environment", "growth_volume",
  "Volume growth (t2 - t1, cm3)",
  "Volume growth by canopy"
)

p_volume_microsite <- plot_growth(
  df.growth.volume.eda, "microsite", "growth_volume",
  "Volume growth (t2 - t1, cm3)",
  "Volume growth by microsite"
)

p_volume_ratio <- plot_growth(
  df.growth.volume.eda, "quercus_sp", "log_ratio_volume",
  "log(t2 / t1)",
  "Relative volume change by species"
)

fig_volume <- (p_volume_species | p_volume_environment) /
  (p_volume_microsite | p_volume_ratio) +
  plot_annotation(
    title = "Quercus volume growth exploratory analysis",
    subtitle = "Absolute change (t2 - t1) and log ratio by species, canopy and microsite"
  )

ggsave(
  filename = "08-img/quercus_growth_volume_distributions.png",
  plot = fig_volume,
  width = 13,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "08-img/quercus_growth_volume_distributions.pdf",
  plot = fig_volume,
  width = 297,
  height = 210,
  units = "mm"
)

p_pairs <- ggplot(
  df.growth.volume.eda,
  aes(x = t1, y = t2, colour = quercus_sp)
) +
  geom_line(
    data = tibble(
      t1 = range(df.growth.volume.eda$t1),
      t2 = range(df.growth.volume.eda$t1)
    ),
    aes(x = t1, y = t2),
    inherit.aes = FALSE,
    linetype = "dashed",
    colour = "grey45"
  ) +
  geom_point(alpha = 0.65, size = 1.8) +
  facet_wrap(~Environment) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Initial volume (t1, cm3)",
    y = "Final volume (t2, cm3)",
    colour = "Species",
    title = "Quercus volume: initial vs final",
    subtitle = "Dashed line indicates no change"
  ) +
  theme_light()

ggsave(
  filename = "08-img/quercus_growth_volume_pairs.png",
  plot = p_pairs,
  width = 10,
  height = 5,
  dpi = 300
)

p_initial_size <- ggplot(
  df.growth.volume.eda,
  aes(x = t1, y = growth_volume, colour = quercus_sp)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
  geom_point(alpha = 0.65, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_wrap(~Environment) +
  scale_x_log10() +
  labs(
    x = "Initial volume (t1, cm3)",
    y = "Volume growth (t2 - t1, cm3)",
    colour = "Species",
    title = "Quercus volume growth vs initial size"
  ) +
  theme_light()

ggsave(
  filename = "08-img/quercus_growth_initial_size.png",
  plot = p_initial_size,
  width = 10,
  height = 5,
  dpi = 300
)

p_height_species <- plot_growth(
  df.growth.twig.eda, "quercus_sp", "height_growth",
  "Height growth (t2 - t1, cm)",
  "Height growth by species"
)

p_diam_species <- plot_growth(
  df.growth.twig.eda, "quercus_sp", "diam_growth",
  "Diameter growth (t2 - t1, mm)",
  "Diameter growth by species"
)

p_height_environment <- plot_growth(
  df.growth.twig.eda, "Environment", "height_growth",
  "Height growth (t2 - t1, cm)",
  "Height growth by canopy"
)

p_diam_environment <- plot_growth(
  df.growth.twig.eda, "Environment", "diam_growth",
  "Diameter growth (t2 - t1, mm)",
  "Diameter growth by canopy"
)

fig_twig <- (p_height_species | p_diam_species) /
  (p_height_environment | p_diam_environment) +
  plot_annotation(
    title = "Quercus twig growth exploratory analysis",
    subtitle = "Height and diameter increments by species and canopy"
  )

ggsave(
  filename = "08-img/quercus_growth_twig_increments.png",
  plot = fig_twig,
  width = 13,
  height = 9,
  dpi = 300
)

p_negative <- df.growth.volume.eda |>
  group_by(quercus_sp, Environment) |>
  summarise(
    n = n(),
    pct_negative = mean(negative_growth),
    .groups = "drop"
  ) |>
  ggplot(aes(x = quercus_sp, y = pct_negative, fill = Environment)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Species",
    y = "Individuals with negative volume growth",
    fill = "Canopy",
    title = "Negative volume growth by species and canopy"
  ) +
  theme_light()

ggsave(
  filename = "08-img/quercus_growth_negative.png",
  plot = p_negative,
  width = 8,
  height = 5,
  dpi = 300
)

cat("Quercus growth EDA completed.\n")
cat("Volume growth rows:", nrow(df.growth.volume.eda), "\n")
cat("Negative volume growth:", sum(df.growth.volume.eda$growth_volume < 0), "\n")
cat("Twig growth rows:", nrow(df.growth.twig.eda), "\n")
cat("Negative height growth:", sum(df.growth.twig.eda$height_growth < 0), "\n")
cat("Negative diameter growth:", sum(df.growth.twig.eda$diam_growth < 0), "\n")

print(summ.quercus.growth, n = Inf)
