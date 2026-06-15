# Paquetes necesarios
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(dplyr)
library(purrr)

# Reporte
cat("Collecting data from all Excel files in the base path\n")

# --- Ajusta aquí la ruta base si hace falta
base_dir <- "00-data/"

# 1) Listar solo .xlsx (recursivo)
xlsx_files <- list.files(path = base_dir,
                         pattern = "\\.xlsx$",
                         recursive = TRUE,
                         full.names = TRUE) %>%
  # eliminar ficheros temporales de Excel que empiezan por ~$
  discard(~ grepl("^~\\$", basename(.x)))

# --- Funciones dedicadas a la lectura de todos los datos

# helper que limpia una columna numérica escrita como texto
clean_to_numeric <- function(x) {
  # mantener NAs
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  # trim
  x <- str_trim(x)
  # eliminar espacios finos y caracteres raros (ej. NBSP)
  x <- str_replace_all(x, "[[:space:]]+", "")
  # reemplazar coma por punto (convierte "24,7" -> "24.7")
  x <- str_replace_all(x, ",", ".")
  # eliminar cualquier carácter que no sea dígito, punto o signo menos
  x <- str_replace_all(x, "[^0-9\\.\\-]", "")
  # convertir a numérico
  suppressWarnings(as.numeric(x))
}

read_lista_sheet <- function(path) {
  sheets <- excel_sheets(path)
  sheet_match <- sheets[tolower(sheets) == "lista"]
  if (length(sheet_match) >= 1) {
    sheet_to_use <- sheet_match[1]
  } else {
    stop(sprintf(
      "No sheet named 'Lista' (case-insensitive) was found in '%s'. Available sheets: %s",
      path,
      paste(sheets, collapse = ", ")
    ))
  }
  
  df_raw <- read_excel(path, sheet = sheet_to_use, col_names = TRUE, col_types = "text")
  df_raw <- janitor::clean_names(df_raw)
  
  # identificar columnas por patrón
  col_tiempo <- names(df_raw)[str_detect(names(df_raw), "tiemp|time")]
  col_temp   <- names(df_raw)[str_detect(names(df_raw), "temper")]
  col_hum    <- names(df_raw)[str_detect(names(df_raw), "humedad|hum|rh")]
  col_no     <- names(df_raw)[str_detect(names(df_raw), "^no\\b|^no$|^no\\.")]
  
  sel <- intersect(c(col_no, col_tiempo, col_temp, col_hum), names(df_raw))
  if (length(sel) == 0) {
    stop(sprintf(
      "In file '%s' (sheet '%s'), no expected columns were identified. Available columns: %s",
      basename(path),
      sheet_to_use,
      paste(names(df_raw), collapse = ", ")
    ))
  }
  
  df <- df_raw %>% select(all_of(sel))
  
  # renombrar según lo que haya
  new_names <- c()
  if (length(col_no))     new_names <- c(new_names, "no")
  if (length(col_tiempo)) new_names <- c(new_names, "tiempo")
  if (length(col_temp))   new_names <- c(new_names, "temperatura")
  if (length(col_hum))    new_names <- c(new_names, "humedad")
  names(df) <- new_names[1:ncol(df)]
  
  # parseos robustos
  df2 <- df %>%
    mutate(
      origen_archivo = basename(path),
      carpeta = basename(dirname(path)),
      aparato = str_extract(origen_archivo, "(?i)Aparato\\s*\\d+"),
      aparato_num = str_extract(origen_archivo, "\\d+")
    )
  
  # convertir tiempo si existe (intentar varios formatos)
  if ("tiempo" %in% names(df2)) {
    df2 <- df2 %>%
      mutate(
        tiempo_raw = tiempo,
        tiempo = suppressWarnings( lubridate::ymd_hms(tiempo_raw, tz = "Europe/Madrid") )
      )
    # si ymd_hms falla, intentar parse_date_time con formatos comunes
    bad_time_idx <- which(is.na(df2$tiempo) & !is.na(df2$tiempo_raw))
    if (length(bad_time_idx) > 0) {
      df2$tiempo[bad_time_idx] <- suppressWarnings(
        parse_date_time(df2$tiempo_raw[bad_time_idx],
                        orders = c("Ymd HMSp", "Ymd HMS", "Y-m-d H:M:S", "d/m/Y H:M:S"),
                        tz = "Europe/Madrid")
      )
    }
  }
  
  # convertir temperatura y humedad con la función robusta
  if ("temperatura" %in% names(df2)) {
    df2 <- df2 %>%
      mutate(
        temperatura_raw = temperatura,
        temperatura = clean_to_numeric(temperatura_raw)
      )
  }
  if ("humedad" %in% names(df2)) {
    df2 <- df2 %>%
      mutate(
        humedad_raw = humedad,
        humedad = clean_to_numeric(humedad_raw)
      )
  }
  
  # diagnóstico: si tras el parseo hay muchos NA en temperatura/humedad, mostrar ejemplo y lanzar warning
  if ("temperatura" %in% names(df2)) {
    n_total <- sum(!is.na(df2$temperatura_raw))
    n_parsed <- sum(!is.na(df2$temperatura))
    if (n_total > 0 && n_parsed == 0) {
      stop(sprintf("In '%s', no temperatures could be parsed. Raw examples: %s",
                   basename(path),
                   paste(head(unique(df2$temperatura_raw), 5), collapse = ", ")))
    } else if (n_total > 0 && n_parsed / n_total < 0.5) {
      warning(sprintf("In '%s', %d/%d temperatures were parsed (%.0f%%). Raw examples: %s",
                      basename(path), n_parsed, n_total, 100 * n_parsed / n_total,
                      paste(head(unique(df2$temperatura_raw[1:10]), 5), collapse = ", ")))
    }
  }
  
  # permitir tablas con 0 filas (avisar) pero no devolver data.frame sin columnas
  if (ncol(df2) == 0) {
    stop(sprintf("The result read from '%s' (sheet '%s') ended up with no useful columns."
, basename(path), sheet_to_use))
  }
  if (nrow(df2) == 0) {
    warning(sprintf("The table read from '%s' (sheet '%s') has 0 rows.", basename(path), sheet_to_use))
  }
  
  # devolver columnas ordenadas (mantenemos las raw si quieres investigar)
  df2 %>% select(any_of(c("no","tiempo","temperatura","humedad","origen_archivo","carpeta","aparato","aparato_num",
                          "tiempo_raw","temperatura_raw","humedad_raw")), everything())
}

# --- ########################################

cat("Files found:", length(xlsx_files), "\n")

if (length(xlsx_files) == 0) {
  stop("No .xlsx files were found in base_dir")
}

# ejecutar la lectura de cada archivo con safely para capturar errores por archivo
results <- purrr::map(xlsx_files, ~ purrr::safely(read_lista_sheet)(.x))

# identificar índices con error no nulo
errors_idx <- which(purrr::map_lgl(results, ~ !is.null(.x$error)))

if (length(errors_idx) > 0) {
  err_msgs <- purrr::map_chr(errors_idx, function(i) {
    file <- xlsx_files[i]
    err  <- results[[i]]$error
    paste0(basename(file), ": ", conditionMessage(err))
  })
  stop(
    sprintf(
      "Read interrupted. %d file(s) with errors were found:\n%s",
      length(err_msgs),
      paste(err_msgs, collapse = "\n")
    )
  )
}

# extraer resultados no nulos y combinarlos
success_list <- purrr::map(results, "result") %>% purrr::compact()

if (length(success_list) == 0) {
  stop("No tables with rows/columns could be read. Check the files and the read_lista_sheet() function.")
}

combined <- dplyr::bind_rows(success_list)

cat("Combined files:", length(success_list), "\n")
cat("Total rows:", nrow(combined), " Columns:", ncol(combined), "\n")
# print(dplyr::glimpse(combined))
print(head(combined, 6))

readr::write_csv(combined, file.path(base_dir, "combined_dataloggers.csv"))
# saveRDS(combined, file.path(base_dir, "combined_dataloggers.rds"))

# Limpiar objetos y funciones no necesarios
rm("results", "success_list", "base_dir", "errors_idx", "xlsx_files", "clean_to_numeric", "read_lista_sheet")
