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

## 1. Bootstrap por separado: controles y tratamientos ----

bootstrap_survival_v2 <- function(df) {
  
  # Separar controles (open) y tratamientos
  df_controls    <- df |> filter(microsite == "open")
  df_treatments  <- df |> filter(microsite != "open")
  
  # --- Controles: un único bootstrap compartido ---
  boot_controls <- df_controls |>
    group_by(Environment, microsite, surv_date) |>
    group_modify(~ {
      boot <- slice_sample(.x, n = nrow(.x), replace = TRUE)
      boot$Individual <- paste0(boot$Individual, "_", boot$quercus_sp, "_", seq_len(nrow(boot)))
      boot
    }) |>
    ungroup()
  
  # --- Tratamientos: bootstrap dentro de cada quercus_sp ---
  boot_treatments <- df_treatments |>
    group_by(Environment, microsite, surv_date, quercus_sp) |>  # añadimos quercus_sp
    group_modify(~ {
      boot <- slice_sample(.x, n = nrow(.x), replace = TRUE)
      boot$Individual <- paste0(boot$Individual, "_", boot$quercus_sp, "_", seq_len(nrow(boot)))
      boot
    }) |>
    ungroup()
  
  # Devolver lista para usar los controles compartidos
  list(
    controls   = boot_controls,
    treatments = boot_treatments
  )
  
}

## 2. Calcular RII por quercus_sp, con controles compartidos ----

calc_rii_by_sp <- function(boot_list, comparisons) {
  
  map_dfr(unique(boot_list$treatments$quercus_sp), function(sp) {
    
    # Subset de tratamientos para esta especie
    trt_sp <- boot_list$treatments |> filter(quercus_sp == sp)
    
    # Controles: filtramos también por especie para mantener coherencia
    # (si los controles tienen las dos especies mezcladas, filtramos aquí)
    ctrl_sp <- boot_list$controls |> filter(quercus_sp == sp)
    
    # Reconstruir df para esta especie: tratamientos + sus controles compartidos
    df_sp <- bind_rows(trt_sp, ctrl_sp)
    
    # Calcular RII con las comparisons habituales
    pmap(comparisons, calc_rii, df = df_sp) |>
      bind_rows() |>
      mutate(quercus_sp = sp)
    
  })
  
}

## 3. Una iteración completa ----

bootstrap_once_v2 <- function(df) {
  
  boot_list <- bootstrap_survival_v2(df)
  
  rii_boot  <- calc_rii_by_sp(boot_list, comparisons)
  
  # Resumen ahora incluye quercus_sp
  rii_boot |>
    group_by(surv_date, type_RII, Interacting_species, quercus_sp) |>
    summarise(RII = mean(RII, na.rm = TRUE), .groups = "drop")
  
}

## 4. Bootstrap final: 5000 iteraciones ----

set.seed(123)

boot.results <- map_dfr(
  seq_len(5000),
  function(i) {
    bootstrap_once_v2(df.surv) |>
      mutate(iteration = i)
  }
)

# Change it depending on the quercus species used
write.csv2(boot.results, file = "00-data/boot_RII.csv")




