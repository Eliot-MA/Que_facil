cat("This script loads and prepares survival data from two census dates (2025 and 2026),\n")
cat("cleans and reshapes growth measurements, and computes volume growth for Quercus seedlings.\n\n")

library(dplyr)
library(tidyr)
library(rlang)
library(readr)
library(dplyr)
#library(readxl)
library(ggplot2)
#library(writexl)
library(purrr)

##
# SURVIVAL ----
##
cat("Loading and merging survival data from both census dates...\n")

library(tidyverse)

# Cargar supervivencia
df.surv1 <- read.csv2("00-data/raw/vol_par_surv.csv")
df.surv2 <- read.csv2("00-data/raw/sup_growth_may2026.csv")


# Prepare survival for 2025
df.surv1 <- df.surv1 |> 
  dplyr::select(micrositio, ambiente, especie_facilitadora, especie_facilitada, # Categorical var
                supervivencia, fecha_supervivencia # survival, response
  ) |> 
  dplyr::rename(
    Individual = micrositio,
    Environment = ambiente,
    microsite  = especie_facilitadora, 
    quercus_sp = especie_facilitada, 
    survival   = supervivencia, 
    surv_date  = fecha_supervivencia
  ) |> 
  mutate(microsite = recode(microsite, 
                            "claro" = "open", 
                            "Cistus sp." = "Cistus ladanifer"),
         Environment      = recode(Environment, 
                                   "abierto" = "gap", 
                                   "pinar" = "pine canopy"),
         Environment      = na_if(Environment, "")
  ) |> unique()

# Prepare survival for 2026

df.surv2 <- df.surv2 |> 
  # Select variables of interest in proper order
  dplyr::select(Arbusto, Ambiente, Especie.arbusto, Quercinea, 
                Supervivencia, Fecha) |> 
  # Rename variables
  dplyr::rename(
    Individual = Arbusto,
    Environment = Ambiente,
    microsite  = Especie.arbusto, 
    quercus_sp = Quercinea, 
    survival   = Supervivencia, 
    surv_date  = Fecha
    # Erase "0" in some individual names (A01, A02...)
  ) |> 
  mutate(
    Individual = sub("([ACJR])0+([1-9][0-9]*)$", "\\1\\2", Individual), 
    microsite = recode(microsite, 
                       "claro" = "open", 
                       "Cistus sp." = "Cistus ladanifer"),
    Environment      = recode(Environment, 
                              "abierto" = "gap", 
                              "pinar" = "pine canopy"),
    Environment      = na_if(Environment, "")
  )

df.surv <- rbind(df.surv1, df.surv2)

# Eliminate J53_bis, causes problems
df.surv <- df.surv |> filter(Individual != "J53_bis")

cat("Survival data merged: n =", nrow(df.surv), "rows,",
    n_distinct(df.surv$Individual), "unique individuals\n\n")

glimpse(df.surv)

##
# GROWTH
##

## ACTUALIZAR PARA INCLUIR FECHA DE MEDICIÓN !!!!!!!!!!!!!!!!!!!!!

cat("\n Loading and reshaping growth data...\n")

rD.growth <- read.csv2(file = "00-data/raw/growth.csv")

## Aspects to change in this dataset ----

library(dplyr)
library(tidyr)
library(stringr)


# 1. Renombrar columnas a un esquema consistente: [H/D]_<rama>_t<tiempo>
#    Vtallo -> V_t1 / V_t2

rD.growth2 <- rD.growth %>%
  mutate(row_id = row_number()) %>%
  rename(
    Individual      = Arbusto,
    quercus_position = Posición,
    quercus_sp      = Especie,
    Environment     = Ambiente,
    quercus_tag     = Individuo,
    observations    = Observaciones,
    H_1_t1 = `h1..cm.`, D_1_t1 = `D1..mm.`,
    H_2_t1 = `h2..cm.`, D_2_t1 = `D2..mm.`,
    H_3_t1 = `h3..cm.`, D_3_t1 = `D3..mm.`,
    H_4_t1 = H4,        D_4_t1 = D4,
    H_1_t2 = H1.2,      D_1_t2 = D1.2,
    H_2_t2 = H2.2,      D_2_t2 = D2.2,
    H_3_t2 = H3.2,      D_3_t2 = D3.2,
    H_4_t2 = H4.2,      D_4_t2 = D4.2,
    V_t1   = `Vtallo..cm3.`,
    V_t2   = `Vtallo.2.Cm.`
  )


# 2. Función de limpieza: pasa todo a character, quita "KO" y "",
#    sustituye coma decimal por punto, convierte a numérico

clean_num <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- na_if(x, "")
  x <- na_if(x, "KO")
  x <- str_replace(x, ",", ".")
  as.numeric(x)
}

rD.growth2 <- rD.growth2 %>%
  mutate(across(matches("^[HD]_\\d_t\\d$|^V_t\\d$"), clean_num))


# 3. Pivotar altura y diámetro juntos (mismo id de rama)

long_hd <- rD.growth2 %>%
  select(row_id, Individual, quercus_position, quercus_sp, Environment,
         quercus_tag, observations, matches("^[HD]_\\d_t\\d$")) %>%
  pivot_longer(
    cols = matches("^[HD]_\\d_t\\d$"),
    names_to = c(".value", "id_rama", "stage"),
    names_pattern = "([HD])_(\\d)_t(\\d)"
  ) %>%
  rename(height = H, diam = D) %>%
  mutate(
    growth_stage = paste0("t", stage),
    id_twig      = id_rama,
    Environment  = recode(str_trim(Environment), "Abierto" = "gap", "Pinar" = "pine canopy"),
    Environment  = na_if(Environment, "")
  ) %>%
  select(-id_rama, -stage)


# 4. Pivotar el volumen (un solo valor por individuo y tiempo,
#    no depende de la rama)

long_v <- rD.growth2 %>%
  select(row_id, V_t1, V_t2) %>%
  pivot_longer(
    cols = starts_with("V_"),
    names_to = "growth_stage",
    names_prefix = "V_",
    values_to = "volume"
  )


# 5. Unir alturas/diámetros con volumen y dar formato final

rD.growth.long <- long_hd %>%
  left_join(long_v, by = c("row_id", "growth_stage")) %>%
  mutate(across(c(Individual, quercus_position, quercus_sp, Environment,
                  quercus_tag, id_twig, growth_stage), as.factor)) %>%
  select(Individual, quercus_position, quercus_sp, Environment, quercus_tag,
         height, id_twig, diam, volume, growth_stage, observations)


# 6. Eliminar filas "vacías": ramas que nunca existieron, donde
#    height y diam son NA a la vez

rD.growth.long <- rD.growth.long %>%
  filter(!(is.na(height) & is.na(diam)))

df.growth <- rD.growth.long

# 7. Creat id per quercus individual

df.growth <- df.growth |> 
  mutate(
    id_quercus = paste0(
      case_when(
        quercus_sp == "Quercus ilex" ~ "E", 
        quercus_sp == "Quercus faginea" ~ "Q"), 
      quercus_tag)
  )

# 8. Calculate growth

df.growth.volume <- df.growth |> 
  select(Individual, quercus_position, quercus_sp, Environment,
         id_quercus, volume, growth_stage) |> 
  unique() |> 
  filter(quercus_sp != "") |> 
  drop_na()

df.growth.volume.diff <- df.growth.volume |> 
  select(id_quercus, Individual, quercus_position, quercus_sp, Environment,
         growth_stage, volume) |> 
  pivot_wider(names_from = growth_stage, values_from = volume) |> 
  mutate(
    growth_volume = t2 - t1, 
    microsite = factor(
      case_when(
        substring(as.character(Individual), 1, 1) == "A" ~ "Genista scorpius", 
        substring(as.character(Individual), 1, 1) == "C" ~ "open", 
        substring(as.character(Individual), 1, 1) == "J" ~ "Cistus ladanifer", 
        substring(as.character(Individual), 1, 1) == "R" ~ "Rosa canina", 
        TRUE ~ NA
      ),
      levels = c("open", "Rosa canina", "Cistus ladanifer", "Genista scorpius")
    )
  ) |> 
  filter(!is.na(growth_volume))

glimpse(df.growth)
glimpse(df.growth.volume)
glimpse(df.growth.volume.diff)

cat("\nScript completed successfully.\n\n")
cat("Main datasets created:\n")
cat("  - df.surv:", nrow(df.surv), "rows | survival from census 2025 and 2026\n")
cat("  - df.growth:", nrow(df.growth), "rows | long-format growth (height, diameter, volume per branch and time)\n")
cat("  - df.growth.volume:", nrow(df.growth.volume), "rows | volume per individual and time\n")
cat("  - df.growth.volume.diff:", nrow(df.growth.volume.diff), "rows | volume growth (t2 - t1) per individual\n")





# ggplot(df.growth.volume.diff, aes(x = quercus_sp, y = growth_volume)) +
#   geom_violin() +
#   geom_jitter(width = .1, alpha = .1) +
#   geom_boxplot(width = .1, alpha = .5)
# 
# 
# ggplot(df.growth.volume.diff, aes(x = Environment, y = growth_volume)) +
#   geom_violin() +
#   geom_jitter(width = .1, alpha = .1) +
#   geom_boxplot(width = .1, alpha = .5)
