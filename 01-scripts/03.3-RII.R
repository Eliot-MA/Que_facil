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

rii.all |>
  group_by(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species
  ) |>
  summarise(
    minimo = min(mean, na.rm = TRUE), 
    q25 = quantile(probs = .25, mean, na.rm = TRUE), 
    mediana = quantile(probs = .5, mean, na.rm = TRUE), 
    media = mean(mean, na.rm = TRUE), 
    q75 = quantile(probs = .75, mean, na.rm = TRUE), 
    maximo = max(mean, na.rm = TRUE)
  )

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

ggplot(aggID.df.rii.qi |> filter(surv_date == "27/05/2026"), 
       aes(x = mean)) +
  geom_histogram(bins = 30) +
  #geom_density() +
  coord_cartesian(xlim = c(-1, 1)) +
  facet_grid(Interacting_species~type_RII)

table(
  aggID.df.rii.qi$mean, 
  aggID.df.rii.qi$Interacting_species, 
  aggID.df.rii.qi$type_RII, 
  aggID.df.rii.qi$surv_date
)

aggID.df.rii.qi |>
  filter(surv_date == "27/05/2026") |> 
  group_by(Interacting_species, type_RII, mean) |> 
  count(mean)

idA <- 1:5 
A <- sample(c(0, 1), size = 5, replace = TRUE)
B <- sample(c(0, 1), size = 5, replace = TRUE)

ejemplo <- expand.grid(A = A, B = B) |> 
  mutate(idA = rep(idA, 5),
    RII_orig = if_else(A + B == 0, 0, (A - B) / (A + B)),
         RII_summ = A - B)

media_B <- mean(ejemplo$B)
ejemplo |> 
  mutate(media_B = media_B, 
         A_media_B = A - media_B) |> 
  group_by(idA) |> 
  summarise(
    media_RII_orig = mean(RII_orig), 
    media_RII_summ = mean(RII_summ), 
    media_RII_summ2 = mean(A_media_B)
  )



# --- Intentando entender qué pasa
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


# Intentado replicar lo que me ocurre a mí
rii.qi |> 
  filter(env_A == "gap", ms_A == "Cistus ladanifer") |> 
ggplot(aes(x = mean)) +
  geom_histogram() +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  coord_cartesian(xlim = c(-1, 1))

rii.qi |> 
  filter(env_A == "gap", ms_A == "Cistus ladanifer", surv_date == "27/05/2026") |> 
  group_by(
  type_RII,
  Interacting_species
) |>
  summarise(
    minimo = min(mean, na.rm = TRUE), 
    q25 = quantile(probs = .25, mean, na.rm = TRUE), 
    mediana = quantile(probs = .5, mean, na.rm = TRUE), 
    media = mean(mean, na.rm = TRUE), 
    q75 = quantile(probs = .75, mean, na.rm = TRUE), 
    maximo = max(mean, na.rm = TRUE)
    )

df.surv |> 
  group_by(Environment, microsite, quercus_sp, surv_date) |> 
  summarise(media = mean(survival, na.rm = TRUE)) |> View()

p_a <- as.numeric(
  df.surv |> 
    filter(Environment == "gap" & microsite == "Cistus ladanifer" & quercus_sp == "QI" & surv_date == "27/05/2026") |> 
    reframe(mean(survival, na.rm = TRUE))
)  
  
p_b <- as.numeric(
  df.surv |> 
    filter(Environment == "gap" & microsite == "open" & quercus_sp == "QI" & surv_date == "27/05/2026") |> 
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
resultados_sesgados <- replicate(n = 30, 
                                 calcular_ri_sesgado(p_A = p_a, p_B = p_b))

# Graficamos
df_resultados <- data.frame(media_RII = resultados_sesgados)

ggplot(df_resultados, aes(x = media_RII)) +
  geom_histogram(bins = 4, fill = "tomato", color = "white") +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Distribution of the mean the RII",
       subtitle = paste0("P(A)=", round(p_a, 2), "; P(B)=", round(p_b, 2)),
       x = "Media de RII",
       y = "Frecuencia") +
  theme_minimal()

df_resultados |>
  summarise(
    minimo = min(media_RII, na.rm = TRUE), 
    q25 = quantile(probs = .25, media_RII, na.rm = TRUE), 
    mediana = quantile(probs = .5, media_RII, na.rm = TRUE), 
    media = mean(media_RII, na.rm = TRUE), 
    q75 = quantile(probs = .75, media_RII, na.rm = TRUE), 
    maximo = max(media_RII, na.rm = TRUE)
  )

check_rii <- df.rii.qi |>
  group_by(ind_A, surv_date, type_RII, Interacting_species) |>
  summarise(
    mean_RII = mean(RII, na.rm = TRUE),
    surv_A = first(surv_A),
    mean_surv_B = mean(surv_B, na.rm = TRUE),
    formula = first(surv_A) - mean(surv_B, na.rm = TRUE),
    diff = mean_RII - formula,
    .groups = "drop"
  )

summary(check_rii$diff)

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

## ---

library(ordinal)

modelo <- 
df.rii.qi |>
  filter(surv_date == "27/05/2026") |>
  drop_na(RII) |> 
  mutate(
    RII = factor(
      RII,
      levels = c(-1,0,1),
      ordered = TRUE
    )
  ) |>
  clmm(
    RII ~
      Interacting_species +
      type_RII +
      (1|ind_A) +
      (1|ind_B),
    data = _
  )

xtabs(~ Interacting_species + RII  + surv_date, data = df.rii.qi)
xtabs(~ type_RII + RII, data = df.rii.qi)
xtabs(~ ind_A + RII, data = df.rii.qi) |> head()
xtabs(~ ind_B + RII, data = df.rii.qi) |> head()

df_filtrado <- 
  df.rii.qi |> 
  filter(surv_date == "27/05/2026") |> 
  mutate(RII = factor(
    RII,
    levels = c(-1,0,1),
    ordered = TRUE
  ))

m0 <- clm(RII ~ Interacting_species * type_RII, data = df_filtrado)
m1 <- clmm(RII ~ Interacting_species * type_RII + (1|ind_A), data = df_filtrado)
m2 <- clmm(RII ~ Interacting_species * type_RII + (1|ind_A) + (1|ind_B), data = df_filtrado)

AIC(m0, m1)

summary(m1)

library(emmeans)

emm <- emmeans(m1.1, specs = ~ type_RII | Interacting_species, type = "response")

library(multcomp)

cld(emm)
