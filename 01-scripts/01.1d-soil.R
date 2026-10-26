library(tidyverse)

# Soil data ----
## Raw inputs
soil_files <- c(
  ph = "00-data/raw/soil_ph.csv",
  texture = "00-data/raw/soil_texture.csv",
  nutrients = "00-data/raw/soil_nutrients.csv"
)

missing_soil_files <- soil_files[!file.exists(soil_files)]
if (length(missing_soil_files) > 0) {
  stop(
    "Missing soil input files: ",
    paste(missing_soil_files, collapse = ", ")
  )
}

## pH
rD.soil.ph <- read.csv2(soil_files[["ph"]], header = TRUE)

df.soil.ph <- rD.soil.ph |>
  rename(Individual = CODIGO.MUESTRA) |>
  mutate(
    Environment = case_when(
      as.numeric(substring(Individual, 2, 3)) > 30 ~ "pine canopy",
      TRUE ~ "gap"
    ),
    microsite = "open"
  )

## Texture
rD.soil.texture <- read.csv2(soil_files[["texture"]])

df.soil.texture <- rD.soil.texture |>
  rename(
    Individual = CODIGO.MUESTRA,
    prop_silt = X..limo,
    prop_clay = X.arcilla,
    prop_sand = X.arena
  ) |>
  mutate(
    Texture = "sandy loam", # Warning: this works only if all samples are sandy loam
    Environment = case_when(
      as.numeric(substring(Individual, 2, 3)) > 30 ~ "pine canopy",
      TRUE ~ "gap"
    )
  ) |>
  select(Individual, Environment, prop_silt, prop_clay, prop_sand, Texture)

## Nutrients
rD.soil.nutrients <- read.csv2(soil_files[["nutrients"]])

df.soil.nutrients <- rD.soil.nutrients |>
  rename(
    Individual = Codigo.Muestra,
    Phosphorus = Fosforo..ppm.,
    Potassium = Potasio..cmol.kg.,
    Total_MO = X.MO.total,
    N = X.N
  ) |>
  mutate(
    Environment = case_when(
      as.numeric(substring(Individual, 2, 3)) > 30 ~ "pine canopy",
      TRUE ~ "gap"
    ),
    microsite = case_when(
      substring(Individual, 1, 1) == "J" ~ "Cistus ladanifer",
      substring(Individual, 1, 1) == "A" ~ "Genista scorpius",
      substring(Individual, 1, 1) == "R" ~ "Rosa canina",
      substring(Individual, 1, 1) == "C" ~ "open"
    )
  )

attr(df.soil.nutrients$Phosphorus, "units") <- "ppm"
attr(df.soil.nutrients$Potassium, "units") <- "cmol/kg"
attr(df.soil.nutrients$Total_MO, "units") <- "%"
attr(df.soil.nutrients$N, "units") <- "%"

cat("Soil data loaded:\n")
cat("  - df.soil.ph:", nrow(df.soil.ph), "rows\n")
cat("  - df.soil.texture:", nrow(df.soil.texture), "rows\n")
cat("  - df.soil.nutrients:", nrow(df.soil.nutrients), "rows\n")
