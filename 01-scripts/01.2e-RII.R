# source("01-scripts/01.21-survival.R")

# 1. Prepare data ----

# Select variables of interest
df.surv <- df.surv |> 
  select(Individual, Environment, microsite, quercus_sp, survival, surv_date) |> 
  mutate(survival = if_else(survival == 2, 1, survival))

# Create subdatasets per quercus species
df.surv.qf <- df.surv |> 
  filter(quercus_sp == "QF")

df.surv.qi <- df.surv |> 
  filter(quercus_sp == "QI")

# 2. Describe comparisons ----
# Each row: focal group (A) vs reference group (B). 

comparisons <- tibble(
  env_A  = c("pine canopy", "pine canopy",    "pine canopy",
             "gap",   "gap",       "gap",
             "pine canopy"),
  ms_A    = c("Cistus ladanifer",  "Rosa canina",    "Genista scorpius",
              "Cistus ladanifer",  "Rosa canina",    "Genista scorpius",
              "open"),
  env_B  = c("pine canopy", "pine canopy",    "pine canopy",
             "gap",   "gap",       "gap",
             "gap"),
  ms_B    = c("open",         "open",            "open",
              "open",         "open",            "open",
              "open"), 
  type_RII = c("Indirect", "Indirect", "Indirect",
               "Direct",   "Direct",   "Direct",
               "Direct"), 
  Interacting_species = c("Cistus ladanifer", "Rosa canina", "Genista scorpius", 
                          "Cistus ladanifer", "Rosa canina", "Genista scorpius", 
                          "Pinus pinaster")
)

# 3. Compute RII ----
## Create function ----
calc_rii <- function(df, env_A, ms_A, env_B, ms_B, type_RII, Interacting_species) {
  
  # Create focal group
  focal <- df |>
    filter(Environment == env_A, microsite == ms_A) |>
    select(
      ind_A = Individual,
      env_A = Environment,
      ms_A = microsite,
      surv_A = survival,
      surv_date
    )
  
  # Create reference group
  ref <- df |>
    filter(Environment == env_B, microsite == ms_B) |>
    select(
      ind_B = Individual,
      env_B = Environment,
      ms_B = microsite,
      surv_B = survival,
      surv_date
    )
  
  # Inner join allow to make comparison only within the same date
  inner_join(focal, ref, by = "surv_date") |>
    mutate(
      type_RII = type_RII,
      Interacting_species = Interacting_species,
      RII = if_else(
        surv_A + surv_B == 0,
        0,
        (surv_A - surv_B) / (surv_A + surv_B)
      )
    )
}

## Apply function to every comparison and combine ----

df.rii.qi <- pmap(comparisons, calc_rii, df = df.surv.qi) |>
  bind_rows()

df.rii.qf <- pmap(comparisons, calc_rii, df = df.surv.qf) |>
  bind_rows()

## Compute aggregated RII per individual

aggID.df.rii.qi <- df.rii.qi |> 
  group_by(ind_A, surv_date, type_RII, Interacting_species) |> 
  summarise(
    mean = mean(RII, na.rm = TRUE), 
    sd   = sd(RII, na.rm = TRUE)
  )

aggID.df.rii.qf <- df.rii.qf |> 
  group_by(ind_A, surv_date, type_RII, Interacting_species) |> 
  summarise(
    mean = mean(RII, na.rm = TRUE), 
    sd   = sd(RII, na.rm = TRUE)
  )

agg.rii.qi <- 
  aggID.df.rii.qi |> 
  filter_out(surv_date == "12/06/2025") |> 
  group_by(surv_date, type_RII, Interacting_species) |> 
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |> 
  mutate(quercus_sp = "Q. ilex")

agg.rii.qf <- 
  aggID.df.rii.qf |> 
  filter_out(surv_date == "12/06/2025") |> 
  group_by(surv_date, type_RII, Interacting_species) |> 
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |> 
  mutate(quercus_sp = "Q. faginea")

agg.rii <- rbind(agg.rii.qi, agg.rii.qf)


# Asegúrate de ejecutar tu código del gráfico primero:
ggplot(agg.rii, aes(x = media, y = Interacting_species, colour = quercus_sp)) + 
  geom_point(position = position_dodge(width = .5), size = 1.5) +
  geom_errorbarh(aes(xmin = media - se, xmax = media + se), width = .5, position = position_dodge(width = .5)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
  facet_grid(surv_date~type_RII) +
  ylab("") +
  xlab("RII") +
  labs(title = "Direct and indirect RII") +
  coord_cartesian(xlim = c(-.6, .6)) +
  theme_light() +
  theme(legend.position = "bottom")

# Luego, guarda el gráfico:
ggsave("08-img/eda_meanse_rii.png", width = 10, height = 8, dpi = 300)

##
# Bootstrapping approach ----
##

library(tidyverse)

## 1. Bootstrap SOLO sobre controles ----

bootstrap_controls_only <- function(df) {
  
  df_controls   <- df |> filter(microsite == "open")
  df_treatments <- df |> filter(microsite != "open")
  
  # Resamplear solo los controles
  boot_controls <- df_controls |>
    group_by(Environment, microsite, surv_date, quercus_sp) |>
    group_modify(~ {
      boot <- slice_sample(.x, n = nrow(.x), replace = TRUE)
      boot$Individual <- paste0(boot$Individual, "_", seq_len(nrow(boot)))
      boot
    }) |>
    ungroup()
  
  # Devolver focales intactos + controles resampleados
  bind_rows(df_treatments, boot_controls)
  
}

## 2. Calcular RII para TODOS los individuos focales (sin promediar aún) ----

bootstrap_once_v3 <- function(df) {
  
  df_boot <- bootstrap_controls_only(df)
  
  # Calcular RII para cada combinación de comparisons y quercus_sp
  map_dfr(unique(df$quercus_sp), function(sp) {
    
    df_sp <- df_boot |> filter(quercus_sp == sp)
    
    pmap(comparisons, calc_rii, df = df_sp) |>
      bind_rows() |>
      mutate(quercus_sp = sp)
    
  })
  
}

## 3. Bootstrap final: 5000 iteraciones ----
## Se guarda la tabla completa con todos los individuos y todas las iteraciones

set.seed(123)

n_iter <- 500

pb <- cli_progress_bar(
  name   = "Bootstrap",
  total  = n_iter,
  format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_eta_str}"
)

df <- df.surv |> filter(Environment == "gap" & microsite %in% c("Genista scorpius", "open") & quercus_sp == "QI", surv_date == "27/05/2026")

boot.results <- map_dfr(
  seq_len(n_iter),
  function(i) {
    cli_progress_update(id = pb)  # <- pasar el ID explícitamente
    bootstrap_once_v3(df) |>
      mutate(iteration = i)
  }
)

cli_progress_done(id = pb)


boot.results.ind <- boot.results |> 
  group_by(ind_A, iteration) |> 
  summarise(
    boot_RII = mean(RII, na.rm = TRUE), 
    boot_lower_ci = quantile(RII, probs = .025, na.rm = TRUE),
    boot_upper_ci = quantile(RII, probs = .975, na.rm = TRUE)
  )


# Change it depending on the quercus species used
write.csv2(boot.results, file = "00-data/boot_RII.csv")


library(dplyr)
library(ggplot2)

hist(boot.results.ind$boot_RII, breaks = 10)

df.surv |> 
  group_by(Environment, microsite, quercus_sp, surv_date) |> 
  summarise(
    n = n(), 
    n_vivo = sum(survival, na.rm = TRUE)
  ) |> 
  filter(Environment == "gap" & microsite %in% c("Genista scorpius", "open") & quercus_sp == "QI", surv_date == "12/06/2025")

df.surv |> 
  filter(Environment == "gap" & microsite == "Genista scorpius" & quercus_sp == "QI", surv_date == "12/06/2025")

vivos_gap <- df.surv |> 
  filter(Environment == "gap" & microsite == "open" & quercus_sp == "QI", surv_date == "12/06/2025") |> 
  summarise(n_vivo = sum(survival, na.rm = TRUE)) |> pull(n_vivo)

1:vivos_gap
posibles_RII <- c()

for (i in 0:vivos_gap) {
  posibles_RII[i] <- 1 - (i/30)
}

hist(posibles_RII, breaks = 150)

## 1. Calcular estadísticos acumulativos ----

convergence <- boot.results |>
  #group_by(iteration, surv_date, type_RII, Interacting_species, quercus_sp) |>
  group_by(iteration, ind_A) +
  summarise(RII_mean = mean(RII, na.rm = TRUE), .groups = "drop") |>
  arrange(iteration) |>
  #group_by(surv_date, type_RII, Interacting_species, quercus_sp) |>
  group_by(Ind_A) |> 
  mutate(
    cum_mean  = cumsum(RII_mean) / seq_len(n()),
    cum_lower = sapply(seq_len(n()), function(i) quantile(RII_mean[1:i], 0.025)),
    cum_upper = sapply(seq_len(n()), function(i) quantile(RII_mean[1:i], 0.975))
  ) |>
  ungroup()

## 2. Graficar convergencia ----

ggplot(convergence, aes(x = iteration)) +
  geom_ribbon(aes(ymin = cum_lower, ymax = cum_upper), alpha = 0.2, fill = "steelblue") +
  geom_line(aes(y = cum_mean), color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(quercus_sp ~ Interacting_species + type_RII) +
  labs(
    x     = "Número de iteraciones",
    y     = "RII acumulado",
    title = "Convergencia del bootstrap"
  ) +
  theme_bw()

## 1. Calcular estadísticos acumulativos por individuo ----

convergence_ind <- boot.results |>
  filter(!is.na(RII)) |>                        # <- eliminar NAs antes
  arrange(iteration) |>
  group_by(ind_A, surv_date, type_RII, Interacting_species, quercus_sp) |>
  mutate(
    cum_mean  = cumsum(RII) / seq_len(n()),
    cum_lower = sapply(seq_len(n()), function(i) quantile(RII[1:i], 0.025, na.rm = TRUE)),
    cum_upper = sapply(seq_len(n()), function(i) quantile(RII[1:i], 0.975, na.rm = TRUE))
  ) |>
  ungroup()

## 2. Graficar convergencia por individuo ----
convergence_ind |> 
  ggplot(aes(x = iteration)) +
  #geom_ribbon(aes(ymin = cum_lower, ymax = cum_upper), alpha = 0.05, fill = "steelblue") +
  geom_point(aes(y = cum_mean), alpha = .01) +
  geom_line(aes(y = cum_mean), color = "steelblue", alpha = 0.3, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(quercus_sp ~ Interacting_species + type_RII) +
  labs(
    x     = "Número de iteraciones",
    y     = "RII acumulado",
    title = "Convergencia del bootstrap por individuo"
  ) +
  theme_bw()