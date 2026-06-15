# source("01.1.Unir_datasets.R")

###############################################################
#               CORREGIR HORAS EQUIVOCADAS                    #
# Los dataloggers que midieron que midieron durante el 22/07  #
# tienen las horas mal registradas, siendo las horas reales   #
# una hora más.                                               #
###############################################################
cat("Running error-correction code \n")

library(dplyr)
library(lubridate)
library(stringr)

# 1) parsear intentando varios órdenes (dmy y ymd; con y sin segundos)
parsed <- parse_date_time(
  combined$tiempo_raw,
  orders = c("dmy HMS", "dmy HM", "ymd HMS", "ymd HM"),
  tz = "Europe/Madrid"
)

# 2) añadir columna con el parseado y guardar original de tiempo
combined <- combined %>%
  mutate(
    tiempo_raw_parsed = parsed,
    tiempo_orig = tiempo
  )

# 3) aplicar corrección: sumar 1 hora sólo a las filas del 22-07-2025 (según tiempo_raw_parsed)
cat("Correction 1. Add 1 hour to al entries from date 2025-07-22 \n")
combined <- combined %>%
  mutate(
    tiempo = if_else(
      !is.na(tiempo_raw_parsed) & date(tiempo_raw_parsed) == ymd("2025-07-22"),
      tiempo + hours(1),
      tiempo
    )
  )

# 4) contar cuántas filas cambiaron realmente (comparando con tiempo_orig)
n_changed <- sum(combined$tiempo != combined$tiempo_orig, na.rm = TRUE)
cat("1 hour was added to the time of", n_changed, "entries \n")

rm("n_changed", "parsed")
