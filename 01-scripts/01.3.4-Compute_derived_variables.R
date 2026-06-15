# source("01.1.Unir_datasets.R")
# source("01.2.Corregir_errores.R")
# source("01.3.Añadir_arbusto_ambiente.R")

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

# Eliminar objetos no necesarios
rm("e", "es", "HR", "nombres1", "nombres2", "T", "Td", "variables_nuevas")

