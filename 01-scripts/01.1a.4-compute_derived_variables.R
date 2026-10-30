cat("Running code for computing derived variables \n")

# Guardar variables originales
nombres1 <- names(final)

# 1) CALCULAR DÉFICIT DE PRESIÓN DE VAPOR
# Fuente: https://meteo.prevencionincendiosgva.es/Guia/DeficitVaporAgua
T  <- final$temperatura
HR <- final$humedad

## Presión de saturación (es)
es <- 610.94 * exp((17.625 * T) / (T + 243.04))

## Temperatura de rocío (Td)
Td <- 243.04 * (log(HR / 100) + (17.625 * T / (243.04 + T))) / (17.625 - log(HR / 100) - (17.625 * T / (243.04 + T)))

## Calcular presión de vapor (e)
e <- 610.94 * exp((17.625 * Td) / (Td + 243.04))

## Calcular VPD
final$VPD <- es - e

## Calcular VPD en kPA
final$VPD_kPA <- final$VPD / 1000

# Reporte
nombres2 <- names(final)
variables_nuevas <- nombres2[!nombres2 %in% nombres1]
cat("Derived variables computed:", paste(variables_nuevas, collapse = ", "))

# Guardar microclima corregido y final
library(readr)
readr::write_csv2(final, "00-data/processed/microclimate.csv")
cat("Saved: 00-data/processed/microclimate.csv\n")

# Eliminar objetos no necesarios
