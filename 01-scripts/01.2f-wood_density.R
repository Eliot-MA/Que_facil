cat("Loading wood density data \n")

rD.wood_density <- read.csv2("00-data/raw/wood_density.csv")

library(tidyverse)

rD.wood_density |> 
  select(Especie)

df.wood_density <- rD.wood_density |>
  select(Especie, Nº.réplica, 
         Peso.húmedo.arriba..g., 
         Peso.húmedo.abajo..g., 
         Peso.seco.arriba..g.,
         Temperatura..ºC.) |> 
  rename(
    Species = Especie, 
    ID = Nº.réplica, 
    fresh.weight.1 = Peso.húmedo.arriba..g., 
    fresh.weight.2 = Peso.húmedo.abajo..g., 
    dry.weight = Peso.seco.arriba..g., 
    temperature = Temperatura..ºC.
  ) |> 
  mutate(
    Environment = if_else(str_detect(ID, "p"), "pine canopy", "gap"), 
    Volume = fresh.weight.1 - fresh.weight.2, 
    wood.density = dry.weight / Volume
  )

library(readr)
readr::write_csv2(df.wood_density, "00-data/processed/wood_density.csv")
cat("Saved: 00-data/processed/wood_density.csv\n")

cat("Wood density loaded in df.wood_density \n")
glimpse(df.wood_density)

