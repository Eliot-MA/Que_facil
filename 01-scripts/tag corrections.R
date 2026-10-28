library(tidyverse)

rD.tag.checked <- read.csv2("00-data/raw/plantas_revisadas.csv")

# Crear columna de Individual.corrected
## Esta columna contendrá el número de individuo que le corresponde finalmente
## Contendrá:
## El valor de Individuo.2026 si 
## - es distinto de NA y
## - Observaciones es NA
## El valor de Individuo si
## - Individuo es distinto de NA y Individuo.2026 es NA

df.tag.checked <- rD.tag.checked %>%
  rename(Environment = Ambiente, 
         Individual = Arbusto, 
         quercus_sp = Especie, 
         quercus_tag_2025 = Individuo, 
         quercus_tag_2026 = Individuo.2026, 
         quercus_position = Posición, 
         observations = Observaciones) |> 
  mutate(
    quercus_tag_corrected = case_when(
      # When to use quercus_tag_2026
      quercus_tag_2026!="" & quercus_tag_2026 != "Rota" & observations=="" ~ quercus_tag_2026,
      # When to use quercus_tag_2025
      quercus_tag_2026=="" & quercus_tag_2025!="" ~ quercus_tag_2025,
      # En cualquier otro caso, asignar NA
      TRUE ~ NA
    )
  ) |> 
  # Translating observations
  ## ambiguous: tag allow different interpretations
  ## unverifiable: absence of tag information
  mutate(
    observations = case_when(
      observations =="" ~ NA, 
      observations =="doble numero enves" ~ "ambiguous_front_back_mismatch", 
      observations %in% c("duda 7", "Dudas 7") ~ "ambiguous_7",
      observations == "etiqueta perdida" ~ "unverifiable_lost_tag", 
      observations == "planta desaparecida" ~ "unverifiable_lost_plant",
      TRUE ~ NA
    )
  )

##
# Check tags
## 

library(dplyr)

tags_checked <- df.tag.checked |> 
  mutate(
    quercus_tag_corrected = trimws(quercus_tag_corrected),
    quercus_sp = trimws(quercus_sp),
    quercus_position = trimws(quercus_position)
  )

check_tag_identity <- tags_checked |> 
  filter(
    !is.na(quercus_tag_corrected),
    quercus_tag_corrected != ""
  ) |> 
  group_by(Individual, quercus_tag_corrected, quercus_sp) |> 
  summarise(
    n_sp = n_distinct(quercus_sp[!is.na(quercus_sp) & quercus_sp != ""]),
    n_position = n_distinct(
      quercus_position[
        !is.na(quercus_position) & quercus_position != ""
      ]
    ),
    missing_sp = any(is.na(quercus_sp) | quercus_sp == ""),
    missing_position = any(is.na(quercus_position) | quercus_position == ""),
    .groups = "drop"
  ) |> 
  filter(
    n_sp != 1 | n_position != 1 |
      missing_sp | missing_position
  )

# Individuals with more than 1 row (possibly errors)
individuals_to_check <- check_tag_identity |> pull(Individual)

library(dplyr)

df.tag.checked <- df.tag.checked |> 
  mutate(
    # 1. Correct species in tags EX and QJ
    quercus_sp = case_when(
      quercus_tag_corrected == "EX" ~ "Quercus ilex",
      quercus_tag_corrected == "QJ" ~ "Quercus faginea",
      TRUE ~ quercus_sp
    )
  ) |> 
  # 2. Complete empty quercus_positions 
  group_by(Individual) |> 
  mutate(
    quercus_position = case_when(
      quercus_position == "" &
        any(quercus_position == "Izquierda", na.rm = TRUE) &
        !any(quercus_position == "Derecha", na.rm = TRUE) ~ "Derecha",

      quercus_position == "" &
        any(quercus_position == "Derecha", na.rm = TRUE) &
        !any(quercus_position == "Izquierda", na.rm = TRUE) ~ "Izquierda",

      TRUE ~ quercus_position
    )
  ) |> 
  ungroup()

df.tag.checked |> 
  filter(Individual %in% individuals_to_check) |> 
  View()

# quercus tags must be unique inside ecah quercus species
tags_to_check <- df.tag.checked |> 
  group_by(quercus_tag_corrected, quercus_sp, observations) |> 
  summarise(
    n = n(), 
    n_equals_1 = n == 1, .groups = "drop"
  ) |> 
  filter(!n_equals_1) |> 
  pull(quercus_tag_corrected)

df.tag.checked |> filter(quercus_tag_corrected %in% tags_to_check & !is.na(quercus_tag_corrected))

source("01-scripts/01.2a-survival_growth.R")

tags_to_check2 <- df.tag.checked |> 
  filter(quercus_tag_corrected %in% tags_to_check & !is.na(quercus_tag_corrected)) |> 
  mutate(id_quercus = case_when(
    quercus_sp == "Quercus ilex" ~ paste0("E", quercus_tag_corrected),
    quercus_sp == "Quercus faginea" ~ paste0("F", quercus_tag_corrected))) |> 
  pull(id_quercus)

df.growth2 <- df.growth |>
  mutate(to_check = id_quercus %in% tags_to_check2) |>
  pivot_wider(
    id_cols = c(Individual, id_quercus, id_twig, to_check),
    names_from = growth_stage,
    values_from = c(height, diam)
  )

# This two graphs allow us to know that 
# comparing height and diameter between t1 and t2
# do not allow to separate clearly plants with 
# wrong id_quercus tags
ggplot(df.growth2, aes(x = height_t1, y = height_t2)) +
  geom_point(aes(colour = to_check)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~id_twig)

ggplot(df.growth2, aes(x = diam_t1, y = diam_t2)) +
  geom_point(aes(colour = to_check)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~id_twig)
