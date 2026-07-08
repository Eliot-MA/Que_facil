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
cat("Loading survival\n")

# Cargar supervivencia
df.surv1 <- read.csv2("00-data/vol_par_surv.csv")
df.surv2 <- read.csv2("00-data/sup_growth_may2026.csv")


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
                                   "pinar" = "pine canopy")
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
                              "pinar" = "pine canopy")
  )

df.surv <- rbind(df.surv1, df.surv2)
glimpse(df.surv)


##
# GROWTH
## 
cat("Loading growth \n")

rD.growth <- read.csv2(file = "00-data/growth.csv")

## Aspects to change in this dataset ----

library(dplyr)
library(tidyr)
library(stringr)


# 1. Renombrar columnas a un esquema consistente: [H/D]_<rama>_t<tiempo>
#    Vtallo -> V_t1 / V_t2

rD.growth2 <- rD.growth %>%
  mutate(row_id = row_number()) %>%
  rename(
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
  select(row_id, Arbusto, Posición, Especie, Ambiente, Individuo,
         Observaciones, matches("^[HD]_\\d_t\\d$")) %>%
  pivot_longer(
    cols = matches("^[HD]_\\d_t\\d$"),
    names_to = c(".value", "id_rama", "Date"),
    names_pattern = "([HD])_(\\d)_t(\\d)"
  ) %>%
  rename(Height_cm = H, Diameter_mm = D) %>%
  mutate(
    Date       = paste0("t", Date),
    id_height  = id_rama,
    id_diameter = id_rama
  ) %>%
  select(-id_rama)


# 4. Pivotar el volumen (un solo valor por individuo y tiempo,
#    no depende de la rama)

long_v <- rD.growth2 %>%
  select(row_id, V_t1, V_t2) %>%
  pivot_longer(
    cols = starts_with("V_"),
    names_to = "Date",
    names_prefix = "V_",
    values_to = "Volume_cm3"
  )


# 5. Unir alturas/diámetros con volumen y dar formato final

rD.growth.long <- long_hd %>%
  left_join(long_v, by = c("row_id", "Date")) %>%
  mutate(across(c(Arbusto, Posición, Especie, Ambiente, Individuo,
                  id_height, id_diameter, Date), as.factor)) %>%
  select(Arbusto, Posición, Especie, Ambiente, Individuo,
         Height_cm, id_height,
         Diameter_mm, id_diameter,
         Volume_cm3, Date, Observaciones)


# 6. Eliminar filas "vacías": ramas que nunca existieron, donde
#    Height_cm, Diameter_mm y Volume_cm3 son todas NA a la vez

rD.growth.long <- rD.growth.long %>%
  filter(!(is.na(Height_cm) & is.na(Diameter_mm) & is.na(Volume_cm3)))

df.growth <- rD.growth.long

# 7. Creat id per quercus individual

df.growth <- df.growth |> 
  mutate(
    id_quercus = paste0(
      case_when(
        Especie == "Quercus ilex" ~ "E", 
        Especie == "Quercus faginea" ~ "Q"), 
      Individuo)
  )

# 8. Calculate growth

df.growth.volume <- df.growth |> 
  select(Arbusto, Posición, Especie, Ambiente, id_quercus, Volume_cm3, Date) |> 
  unique() |> 
  filter_out(Especie == "") |> 
  drop_na()

df.growth.volume.diff <- df.growth.volume |> 
  select(id_quercus, Arbusto, Posición, Especie, Ambiente, Date, Volume_cm3) |> 
  pivot_wider(names_from = Date, values_from = Volume_cm3) |> 
  mutate(
    growth_volume = t2 - t1, 
    micrositio = factor(
      case_when(
        substring(Arbusto, 1, 1) == "A" ~ "Genista scorpius", 
        substring(Arbusto, 1, 1) == "C" ~ "Claro", 
        substring(Arbusto, 1, 1) == "J" ~ "Cistus ladanifer", 
        substring(Arbusto, 1, 1) == "R" ~ "Rosa canina", 
        TRUE ~ NA
      ),
      levels = c("Claro", "Rosa canina", "Cistus ladanifer", "Genista scorpius")
    )
  ) |> 
  filter(!is.na(growth_volume))

glimpse(df.growth)
glimpse(df.growth.volume)
glimpse(df.growth.volume.diff)





# ggplot(df.growth.volume.diff, aes(x = Especie, y = growth_volume)) +
#   geom_violin() +
#   geom_jitter(width = .1, alpha = .1) +
#   geom_boxplot(width = .1, alpha = .5)
# 
# 
# ggplot(df.growth.volume.diff, aes(x = Ambiente, y = growth_volume)) +
#   geom_violin() +
#   geom_jitter(width = .1, alpha = .1) +
#   geom_boxplot(width = .1, alpha = .5)
