# source("01.1.Unir_datasets.R")
# source("01.2.Corregir_errores.R")

cat("Running treatment variables merging code \n")

# Cargar datos que relacionan el aparato, el arbusto, el ambiente y la fecha
aparato_arbustos_ambiente <- read.csv2(
  "00-data/datos_humedad_temperatura_tamajon.csv",
  fileEncoding = "latin1"
)

# Seleccionar variables de interés
aparato_arbustos_ambiente <- aparato_arbustos_ambiente[,c("ambiente", "micrositio", "individuo", "data_logger", "fecha", "hora_entrada", "hora_salida")]

######################################################
# 1) Preparar fecha_hora_entrada y fecha_hora_salida #
######################################################

library(dplyr)
library(lubridate)

# capturamos la zona horaria de combined$tiempo (si existe)
tz_combined <- attr(combined$tiempo, "tzone")

# creamos las columnas datetime

aparato_arbustos_ambiente <- aparato_arbustos_ambiente %>%
  mutate(
    fecha = as.Date(fecha, format = "%d/%m/%Y"),
    fecha_hora_entrada = as.POSIXct(
      paste(fecha, hora_entrada),
      format = "%Y-%m-%d %H:%M",
      tz = tz_combined
    ),
    fecha_hora_salida = as.POSIXct(
      paste(fecha, hora_salida),
      format = "%Y-%m-%d %H:%M",
      tz = tz_combined
    )
  )

# aparato_arbustos_ambiente <- aparato_arbustos_ambiente %>%
#   mutate(
#     fecha = as.Date(fecha),
#     fecha_hora_entrada = as.POSIXct(
#       paste(fecha, format(hora_entrada, "%H:%M:%S")),
#       tz = tz_combined
#     ),
#     fecha_hora_salida = as.POSIXct(
#       paste(fecha, format(hora_salida, "%H:%M:%S")),
#       tz = tz_combined
#     )
#   )

##########################
# 2) Unión con fuzzyjoin #
##########################

library(fuzzyjoin)

# asegurarse de tipos compatibles
combined2 <- combined %>% mutate(aparato_num = as.numeric(aparato_num))

# seleccionar columnas útiles del segundo dataset
meta <- aparato_arbustos_ambiente %>%
  select(data_logger, ambiente, micrositio, individuo, fecha_hora_entrada, fecha_hora_salida)

# fuzzy_left_join con tres condiciones:
# 1) aparato_num == data_logger
# 2) tiempo > fecha_hora_entrada
# 3) tiempo <= fecha_hora_salida
joined <- fuzzy_left_join(
  combined2, meta,
  by = c("aparato_num" = "data_logger",
         "tiempo"      = "fecha_hora_entrada",
         "tiempo"      = "fecha_hora_salida"),
  match_fun = list(`==`, `>`, `<=`)
)

# limpiar columnas y filas no necesarias: 
final <- joined %>%
  select(individuo, micrositio, ambiente, 
         tiempo, temperatura, humedad, 
         aparato, carpeta) %>% # columnas de interés
  filter(!if_any(c(ambiente, micrositio, individuo), is.na))   # las filas con NA en estas columnas son mediciones que no se realizaron en ningún micrositio

#############################
# 3) Comprobaciones finales #
#############################

# Para mostrar tablas pequeñas usando cat() convertimos la salida a texto:
txt <- function(x, n = 10) {
  capture.output(print(head(x, n = n))) |> paste(collapse = "\n")
}

# 1) tamaños básicos
total_combined <- nrow(combined)
total_joined   <- nrow(joined)   # antes de filtrar NA en ambiente
total_final    <- nrow(final)    # después de filtrar NA en ambiente

cat("=== Summary: length ===\n")
cat("Rows in combined (original): ", total_combined, "\n")
cat("Rows in joined (result fuzzy_left_join, before filtering NA): ", total_joined, "\n")
cat("Rows in final (after selecting columns and filtering NA in ambiente): ", total_final, "\n\n")

# 2) Filas de combined que se asignaron a MÁS DE UNA fila de meta (posible multiasignación/duplicado)
multi_match_counts <- joined %>%
  count(aparato_num, carpeta, no, name = "n_matches") %>%
  filter(n_matches > 1)

n_rows_with_multimatch <- nrow(multi_match_counts)
total_multimatch_instances <- sum(multi_match_counts$n_matches)  # total de coincidencias entre esas filas (suma de matches)

cat("=== MULTI-ASSIGNMENTS ===\n")
cat("Rows in 'combined' with >1 match in meta (possible overlap duplicates): ", n_rows_with_multimatch, "\n")

if(n_rows_with_multimatch > 0) {
  cat("Example rows with multiple matches (n_matches) — up to 6:\n")
  cat(txt(multi_match_counts %>% arrange(desc(n_matches)), n = 6), "\n\n")
} else {
  cat("No rows with multiple matches were detected.\n\n")
}

# Eliminar objetos no necesarios
rm("aparato_arbustos_ambiente", "combined", "combined2", "joined", "meta", "multi_match_counts", "n_rows_with_multimatch", "total_combined", "total_final", "total_joined", "total_multimatch_instances", "tz_combined", "txt")
