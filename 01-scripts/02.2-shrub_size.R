library(moments)
library(GGally)
library(tidyverse)

#############
# LOAD DATA #
#############

# Load data
source("01-scripts/01.1-gps.R")             # gps and elevation info
source("01-scripts/01.2-surv_par_vol.R")    # survival, par and volume
source("01-scripts/01.5-predict_biomass.R") # load biomass

rm(list = setdiff(ls(), c("df.surv.par.vol", "df.shrubs.gps")))

df.shrubs.gps <- df.shrubs.gps |> 
  mutate(
    Elevation_c = scale(Elevation), 
    Individual = as.factor(Individual)
  )

# Merge
df <- merge(
  x = df.shrubs.gps   |> select(Individual, X, Y, Elevation, Elevation_c), 
  y = df.surv.par.vol |> select(Individual, zone, microsite, quercus_sp, height, diam_1, diam_2, survival, surv_date), 
  by = "Individual"
)

# Only data from september

df <- df |> 
  filter(surv_date == "16/09/2025") |> 
  filter(!is.na(survival)) |> 
  unique()

# Compute volume
df <-
df |> 
  mutate(
    volume = (4/3) * pi * height/2 * diam_1/2 * diam_2/2
  )

# Data with only shrubs
df.only.shrubs <- df |> 
  select(Individual, X, Y, Elevation, zone, microsite, height, diam_1, diam_2, volume) |> 
  filter_out(microsite == "gap") |> 
  unique()

#####################################
# UNIVARIANT AND BIVARIANT ANALYSIS #
#####################################

library(ggplot2)
library(ggdist)
library(patchwork) # Para combinar los gráficos

crear_raincloud <- function(data, y_var, x_var = NULL) {
  
  if (is.null(x_var)) {
    # --- Caso 1: Análisis Univariado ---
    p <- ggplot(data, aes(x = 1, y = .data[[y_var]])) + 
      stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
      geom_boxplot(width = .15, outlier.shape = NA) +
      geom_jitter(aes(x = .75), width = .15, alpha = 0.2) +
      theme_light() +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank())
  } else {
    # --- Caso 2: Análisis Bivariado (Zone o Microsite) ---
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) + 
      stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
      geom_boxplot(width = .15, outlier.shape = NA) +
      geom_jitter(width = .15, alpha = .2) +
      theme_light()
  }
  
  return(p)
}

# Lista de variables numéricas a analizar
variables <- c("height", "diam_1", "diam_2", "volume")

# Fila 1: Univariados
lista_univariados <- lapply(variables, function(v) crear_raincloud(df.only.shrubs, v))

# Fila 2: Bivariados por Zone
lista_zone <- lapply(variables, function(v) crear_raincloud(df.only.shrubs, v, "zone"))

# Fila 3: Bivariados por Microsite
lista_microsite <- lapply(variables, function(v) crear_raincloud(df.only.shrubs, v, "microsite"))

# Combinamos todas las listas de gráficos en una sola
todos_los_graficos <- c(lista_univariados, lista_zone, lista_microsite)

# Creamos la composición final
grafico_final <- wrap_plots(todos_los_graficos, ncol = 4) + 
  plot_annotation(
    title = "Shrub dimension exploratory analysis (df.only.shrubs)",
    subtitle = "Row 1: Univariant | Row 2: per zone | Row 3: per microsite",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Mostrar en el panel de RStudio
# print(grafico_final)

ggsave(
  filename = "08-img/shrub_dimensions_A4.pdf", 
  plot = grafico_final, 
  width = 297, 
  height = 210, 
  units = "mm"
)

# Zone*microsite
crear_raincloud_facetado <- function(data, x_var, y_var, facet_var, es_primero = FALSE) {
  
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) + 
    stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.2, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .15, 
      outlier.shape = NA
    ) +
    geom_jitter(
      width = .15, 
      alpha = .2
    ) +
    theme_light() +
    # Facetado dinámico usando la variable elegida
    facet_wrap(as.formula(paste0("~", facet_var)), ncol = 1) +
    coord_flip()
  
  # Si NO es el primer gráfico, limpiamos el eje para que lo comparta con el de la izquierda
  if (!es_primero) {
    p <- p + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank() # Quitamos también los ticks para un acabado más limpio
      ) + 
      xlab("")
  }
  
  return(p)
}

# Variables a iterar
variables <- c("height", "diam_1", "diam_2", "volume")

# Generamos la lista de los 4 gráficos
lista_graficos <- lapply(seq_along(variables), function(i) {
  crear_raincloud_facetado(
    data = df.only.shrubs,
    x_var = "microsite",
    y_var = variables[i],
    facet_var = "zone",
    es_primero = (i == 1) # TRUE solo para el primero ('height')
  )
})

# Unimos los 4 gráficos en 1 sola fila
grafico_final <- wrap_plots(lista_graficos, nrow = 1) + 
  plot_annotation(
    title = "Shrub dimensions per microsite and zone",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Visualizar en RStudio
# print(grafico_final)

ggsave(
  filename = "08-img/shrub_dimensions_zone_microsite_A4.pdf", 
  plot = grafico_final, 
  width = 297, 
  height = 210, 
  units = "mm"
)

# Elevation ~ volume

ggplot(df.only.shrubs, aes(x = Elevation, y = volume)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(zone~microsite) +
  theme_bw()

summary(lm(volume ~ Elevation*microsite*zone, data = df.only.shrubs))

anova(lm(volume ~ Elevation*microsite*zone, data = df.only.shrubs))

df.pca <- df.only.shrubs |> select(Elevation, height, diam_1, diam_2)
pca <- prcomp(df.pca, 
              center = TRUE, 
              scale. = TRUE)

library(factoextra)

fviz_pca_ind(pca)   # individuos
fviz_pca_var(pca)   # variables
fviz_pca_biplot(pca)

#Surv ~ shrub size

## pred.mass

df.surv.vol <- df.surv.par.vol |> 
  select(Individual, zone, microsite, 
         survival, quercus_sp, 
         height, diam_1, diam_2, volume, pred.mass) |> 
  filter(microsite != "gap") |> 
  mutate(survival = if_else(survival == 2, 1, survival)) |> 
  drop_na(survival) |> 
  unique()

library(patchwork)

# --- Plots de pred.mass ---
p1 <- ggplot(df.surv.vol, aes(x = factor(survival), y = pred.mass)) +
  geom_boxplot() + geom_violin(alpha = .2) + geom_jitter(alpha = .2, width = .2) +
  scale_y_log10() + labs(title = "Global")

p2 <- ggplot(df.surv.vol, aes(y = survival, x = pred.mass)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_x_continuous(trans = "log10") + labs(title = "Logística global")

p3 <- ggplot(df.surv.vol, aes(x = factor(survival), y = pred.mass)) +
  geom_boxplot() + geom_violin(alpha = .2) + geom_jitter(alpha = .2, width = .2) +
  scale_y_log10() + facet_grid(zone ~ microsite) + theme_bw() + labs(title = "Por zona y micrositio")

p4 <- ggplot(df.surv.vol, aes(y = survival, x = pred.mass)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_x_continuous(trans = "log10") + facet_grid(zone ~ microsite) + theme_bw() + labs(title = "Logística por zona y micrositio")

# Organización: 2 columnas, fila superior = globales, fila inferior = facetados
figura_mass <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Relation between estimated mass (pred.mass) and quercus survival.",
    subtitle = "Left: distribution per group | Right: logistic model"
  )

ggsave("08-img/patch_survival_mass_zone_microsite.png", figura_mass, width = 14, height = 10, dpi = 300)

## volume

# --- Plots de volume ---
p1 <- ggplot(df.surv.vol, aes(x = factor(survival), y = volume)) +
  geom_boxplot() + geom_violin(alpha = .2) + geom_jitter(alpha = .2, width = .2) +
  scale_y_log10() + labs(title = "Global")

p2 <- ggplot(df.surv.vol, aes(y = survival, x = volume)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_x_continuous(trans = "log10") + labs(title = "Logística global")

p3 <- ggplot(df.surv.vol, aes(x = factor(survival), y = volume)) +
  geom_boxplot() + geom_violin(alpha = .2) + geom_jitter(alpha = .2, width = .2) +
  scale_y_log10() + facet_grid(zone ~ microsite) + theme_bw() + labs(title = "Por zona y micrositio")

p4 <- ggplot(df.surv.vol, aes(y = survival, x = volume)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_x_continuous(trans = "log10") + facet_grid(zone ~ microsite) + theme_bw() + labs(title = "Logística por zona y micrositio")

# Organización: 2 columnas, fila superior = globales, fila inferior = facetados
figura_mass <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Relation between volume and quercus survival.",
    subtitle = "Left: distribution per group | Right: logistic model"
  )

ggsave("08-img/patch_survival_volume_zone_microsite.png", figura_mass, width = 14, height = 10, dpi = 300)
