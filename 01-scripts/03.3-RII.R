library(tidyverse)
library(broom)

source("01-scripts/01.21-survival.R")
source("01-scripts/01.22-RII.R")

# Añadir nombre de especie
rii.qi <- aggID.df.rii.qi |>
  mutate(quercus_sp = "Q. ilex")

rii.qf <- aggID.df.rii.qf |>
  mutate(quercus_sp = "Q. faginea")

rii.all <- bind_rows(rii.qi, rii.qf) |>
  filter(surv_date != "12/06/2025")

# One-sample t-tests against RII = 0

ttest.rii <- rii.all |>
  group_by(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species
  ) |>
  summarise(
    test = list(t.test(mean, mu = 0)),
    .groups = "drop"
  ) |>
  mutate(tidy = map(test, broom::tidy)) |>
  unnest(tidy) |>
  select(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species,
    estimate,
    statistic,
    parameter,
    conf.low,
    conf.high,
    p.value
  ) |>
  arrange(quercus_sp,
          surv_date,
          type_RII,
          Interacting_species)

ttest.rii

ttest.rii <- ttest.rii |>
  mutate(
    interpretation = case_when(
      p.value < 0.05 & estimate > 0 ~ "Facilitation",
      p.value < 0.05 & estimate < 0 ~ "Competition",
      TRUE ~ "Neutral"
    )
  )

ggplot(
  ttest.rii,
  aes(
    x = estimate,
    y = Interacting_species,
    colour = interpretation
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(shape = quercus_sp), size = 3, 
             position = position_dodge(width = 1)) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high, 
      shape = quercus_sp
    ),
    height = .2, 
    position = position_dodge(width = 1)
  ) +
  facet_grid(
    surv_date ~  type_RII
  ) +
  theme_light() +
  labs(
    x = "Mean RII",
    y = "",
    colour = "Effect"
  )

# Check model

library(tidyverse)

normality.tests <-
  rii.all |>
  group_by(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species
  ) |>
  summarise(
    p.shapiro =
      shapiro.test(mean)$p.value,
    .groups = "drop"
  )

normality.tests # Ausencia detectable de normalidad

ggplot(
  rii.all,
  aes(sample = mean)
) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(
    surv_date ~
      quercus_sp +
      type_RII +
      Interacting_species
  )

ggplot(
  rii.all,
  aes(mean)
) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    surv_date ~
      quercus_sp +
      type_RII +
      Interacting_species
  )

ggplot(
  rii.all,
  aes(
    x = Interacting_species,
    y = mean
  )
) +
  geom_violin() +
  geom_boxplot(width = .25, alpha = .5) +
  geom_jitter() +
  facet_grid(
    surv_date ~
      quercus_sp +
      type_RII
  ) +
  coord_flip()

library(ggridges)
ggplot(
  rii.all,
  aes(
    y = Interacting_species,
    x = mean
  )
) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  facet_grid(
    surv_date ~
      quercus_sp +
      type_RII
  )

ggplot(df.rii.qi, aes(x = RII)) +
  geom_histogram() +
  facet_grid(Interacting_species~type_RII)

ggplot(aggID.df.rii.qi, aes(x = mean)) +
  geom_histogram(bins = 30) +
  #geom_density() +
  coord_cartesian(xlim = c(-1, 1)) +
  facet_grid(Interacting_species~type_RII)



# Definimos una función que realiza el cálculo una vez
calcular_ri <- function(n = 30) {
  A <- sample(c(0, 1), size = n, replace = TRUE)
  B <- sample(c(0, 1), size = n, replace = TRUE)
  
  expand.grid(A = A, B = B) |> 
    mutate(
      RII = if_else(A + B == 0, 0, (A - B) / (A + B))
    ) |> 
    summarise(mean_RII = mean(RII)) |> 
    pull(mean_RII)
}

# Repetimos la simulación 1000 veces
# set.seed(123) # Para reproducibilidad
resultados <- replicate(1000, calcular_ri())

# Convertimos a data.frame para graficar
df_resultados <- data.frame(media_RII = resultados)

# Graficamos la distribución
ggplot(df_resultados, aes(x = media_RII)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la media de RII (1000 iteraciones)",
       x = "Media de RII",
       y = "Frecuencia") +
  theme_minimal()


p_a <- as.numeric(
  df.surv |> 
    filter(Environment == "gap" & microsite == "open") |> 
    reframe(mean(survival, na.rm = TRUE))
)

p_b <- as.numeric(
  df.surv |> 
    filter(Environment == "gap" & microsite == "Cistus ladanifer") |> 
    reframe(mean(survival, na.rm = TRUE))
)

# Modificamos la función para incluir 'p_B', la probabilidad de obtener un 1 en B
calcular_ri_sesgado <- function(n = 30, p_A = 0.5, p_B = 0.5) {
  A <- sample(c(0, 1), size = n, replace = TRUE, prob = c(1 - p_A, p_A))
  # Usamos p_B para sesgar la muestra (p_B es la prob. de obtener 1)
  B <- sample(c(0, 1), size = n, replace = TRUE, prob = c(1 - p_B, p_B))
  
  expand.grid(A = A, B = B) |> 
    mutate(
      RII = if_else(A + B == 0, 0, (A - B) / (A + B))
    ) |> 
    summarise(mean_RII = mean(RII)) |> 
    pull(mean_RII)
}

# Ejecutamos la simulación con un sesgo hacia los unos en B (p. ej., 70% de unos)
set.seed(123)
resultados_sesgados <- replicate(90, calcular_ri_sesgado(p_A = p_a, p_B = p_b))

# Graficamos
df_resultados <- data.frame(media_RII = resultados_sesgados)

ggplot(df_resultados, aes(x = media_RII)) +
  geom_histogram(bins = 4, fill = "tomato", color = "white") +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Distribución de la media de RII con sesgo en A y B (p=0.7)",
       x = "Media de RII",
       y = "Frecuencia") +
  theme_minimal()

#---




library(tidyverse)

calcular_ri_sesgado <- function(n = 30, p_A = 0.5, p_B = 0.5) {
  A <- sample(c(0, 1), size = n, replace = TRUE, prob = c(1 - p_A, p_A))
  B <- sample(c(0, 1), size = n, replace = TRUE, prob = c(1 - p_B, p_B))
  
  expand.grid(A = A, B = B) |> 
    mutate(
      RII = if_else(A + B == 0, 0, (A - B) / (A + B))
    ) |> 
    summarise(mean_RII = mean(RII)) |> 
    pull(mean_RII)
}

# Todas las combinaciones de probabilidades
probs <- c(0.01, 0.25, 0.50, 0.75, 0.99)

# Generamos todos los resultados en un data.frame largo
set.seed(123)
df_grid <- expand.grid(p_A = probs, p_B = probs) |>
  rowwise() |>
  mutate(
    resultados = list(replicate(90, calcular_ri_sesgado(n = 30, p_A = p_A, p_B = p_B)))
  ) |>
  unnest(resultados) |>
  rename(media_RII = resultados)

# Etiquetas legibles para el facet
df_grid <- df_grid |>
  mutate(
    label_A = factor(paste0("p(A=1) = ", p_A), levels = paste0("p(A=1) = ", probs)),
    label_B = factor(paste0("p(B=1) = ", p_B), levels = paste0("p(B=1) = ", probs))
  )

# Grid de gráficos: columnas = p_A, filas = p_B
ggplot(df_grid, aes(x = media_RII)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  facet_grid(
    rows = vars(label_B),
    cols = vars(label_A)
  ) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(
    title = "Distribución de la media de RII según p(A=1) y p(B=1)",
    subtitle = "500 iteraciones por combinación · n = 30",
    x = "Media de RII",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text = element_text(size = 7, face = "bold"),
    panel.spacing = unit(0.4, "lines"),
    plot.title = element_text(face = "bold")
  )
