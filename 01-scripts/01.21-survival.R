library(dplyr) 
library(tidyr) 
library(rlang)
library(readr)
library(dplyr) 
#library(readxl)
library(ggplot2)
#library(writexl)
library(purrr)

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
