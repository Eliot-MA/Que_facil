cat("==Running script for Quercus ID correction==\n")
cat("ID tags were verified in the field to resolve some discrepancies\n")

# Setup -------------------------------------------------------------------
library(tidyverse)

# 1) Load raw data ---------------------------------------------------------
rD.tag.checked <- read.csv2("00-data/raw/plantas_revisadas.csv")

# 2) Rename columns and build quercus_tag_corrected ------------------------
df.tag.checked <- rD.tag.checked %>%
  rename(
    Environment      = Ambiente,
    Individual       = Arbusto,
    quercus_sp       = Especie,
    quercus_tag_2025 = Individuo,
    quercus_tag_2026 = Individuo.2026,
    quercus_position = Posición,
    observations     = Observaciones
  ) %>%
  mutate(
    quercus_tag_corrected = case_when(
      # Prefer tag from 2026 if present, valid and no observations
      quercus_tag_2026 != "" & quercus_tag_2026 != "Rota" & observations == "" ~ quercus_tag_2026,
      # Use tag from 2025 if 2026 is missing
      quercus_tag_2026 == "" & quercus_tag_2025 != "" ~ quercus_tag_2025,
      # Otherwise assign NA
      TRUE ~ NA_character_
    )
  ) %>%
  # 3) Translate observations ----------------------------------------------
  mutate(
    observations = case_when(
      observations == ""                        ~ NA_character_,
      observations == "doble numero enves"     ~ "ambiguous_front_back_mismatch",
      observations %in% c("duda 7", "Dudas 7") ~ "ambiguous_7",
      observations == "etiqueta perdida"       ~ "unverifiable_lost_tag",
      observations == "planta desaparecida"    ~ "unverifiable_lost_plant",
      TRUE                                     ~ NA_character_
    )
  )

# 4) Normalise values ------------------------------------------------------
tags_checked <- df.tag.checked %>%
  mutate(
    quercus_tag_corrected = trimws(quercus_tag_corrected),
    quercus_sp            = trimws(quercus_sp),
    quercus_position      = trimws(quercus_position)
  )

# 5) Check consistency within the same Individual --------------------------
check_tag_identity <- tags_checked %>%
  filter(!is.na(quercus_tag_corrected), quercus_tag_corrected != "") %>%
  group_by(Individual, quercus_tag_corrected, quercus_sp) %>%
  summarise(
    n_sp            = n_distinct(quercus_sp[!is.na(quercus_sp) & quercus_sp != ""]),
    n_position      = n_distinct(quercus_position[!is.na(quercus_position) & quercus_position != ""]),
    missing_sp      = any(is.na(quercus_sp) | quercus_sp == ""),
    missing_position = any(is.na(quercus_position) | quercus_position == ""),
    .groups         = "drop"
  ) %>%
  filter(n_sp != 1 | n_position != 1 | missing_sp | missing_position)

# Individuals with more than 1 row (possibly errors)
individuals_to_check <- check_tag_identity %>% pull(Individual)

# 6) Apply manual corrections ----------------------------------------------
df.tag.checked <- df.tag.checked %>%
  # 6.1 Correct species for special tags EX and QJ
  mutate(
    quercus_sp = case_when(
      quercus_tag_corrected == "EX" ~ "Quercus ilex",
      quercus_tag_corrected == "QJ" ~ "Quercus faginea",
      TRUE                          ~ quercus_sp
    )
  ) %>%
  # 6.2 Complete empty quercus_position within the same Individual
  group_by(Individual) %>%
  mutate(
    quercus_position = case_when(
      quercus_position == "" &
        any(quercus_position == "Izquierda", na.rm = TRUE) &
        !any(quercus_position == "Derecha",   na.rm = TRUE) ~ "Derecha",
      quercus_position == "" &
        any(quercus_position == "Derecha",   na.rm = TRUE) &
        !any(quercus_position == "Izquierda", na.rm = TRUE) ~ "Izquierda",
      TRUE ~ quercus_position
    )
  ) %>%
  ungroup()

# 7) Detect duplicated tags within the same species ------------------------
tags_to_check <- df.tag.checked %>%
  filter(!is.na(quercus_tag_corrected), quercus_tag_corrected != "") %>%
  group_by(quercus_tag_corrected, quercus_sp, observations) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  pull(quercus_tag_corrected) %>%
  unique()

# Inspect duplicated tags
df.tag.checked %>% 
  filter(quercus_tag_corrected %in% tags_to_check)

# 8) Find missing tag numbers between 1 and 240 per species ---------------
# Quercus ilex
ilex_present <- df.tag.checked %>%
  filter(quercus_sp == "Quercus ilex", !is.na(quercus_tag_corrected), quercus_tag_corrected != "") %>%
  pull(quercus_tag_corrected) %>%
  as.numeric()

ilex_tags <- setdiff(1:240, ilex_present)

# Quercus faginea
faginea_present <- df.tag.checked %>%
  filter(quercus_sp == "Quercus faginea", !is.na(quercus_tag_corrected), quercus_tag_corrected != "") %>%
  pull(quercus_tag_corrected) %>%
  as.numeric()

faginea_tags <- setdiff(1:240, faginea_present)

# 9) Flag doubtful tags (conservative approach) ----------------------------
df.tag.checked <- df.tag.checked %>%
  mutate(
    tag_doubtufll = case_when(
      quercus_sp == "Quercus ilex"   & quercus_tag_corrected %in% as.character(tags_to_check) ~ TRUE,
      quercus_sp == "Quercus ilex"   & quercus_tag_corrected %in% as.character(ilex_tags)     ~ TRUE,
      quercus_sp == "Quercus faginea" & quercus_tag_corrected %in% as.character(faginea_tags) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# 10) Summary for filtering ambiguous/reliable tags -------------------------

ambiguous_tags <- df.tag.checked |>
  filter(tag_doubtufll == TRUE,
         !is.na(quercus_tag_corrected), quercus_tag_corrected != "") |>
  distinct(Individual, quercus_sp, quercus_tag_corrected, observations) |>
  arrange(quercus_sp, quercus_tag_corrected, Individual)

reliable_tags <- df.tag.checked |>
  filter(tag_doubtufll == FALSE,
         !is.na(quercus_tag_corrected), quercus_tag_corrected != "") |>
  distinct(Individual, quercus_sp, quercus_tag_corrected)

unverifiable_tags <- df.tag.checked |>
  filter(is.na(quercus_tag_corrected) | quercus_tag_corrected == "",
         !is.na(observations), observations != "") |>
  distinct(Individual, quercus_sp, Environment, quercus_position, observations) |>
  arrange(quercus_sp, Individual)

# Compact summary
cat("Summary: \n")
cat(sprintf(
  "[TAGS] Reliable: %d | Ambiguous: %d | Unverifiable: %d\n",
  nrow(reliable_tags),
  nrow(ambiguous_tags),
  nrow(unverifiable_tags)
))
cat("Ambiguous and Unverifiable ids will be excluded from growth analyses \n")
cat("Script for Quercus ID correction executed \n")