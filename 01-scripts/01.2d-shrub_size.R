## Shrubs size
rD.vol <- read.csv2("00-data/raw/vol_par_surv.csv")

df.vol <- rD.vol |> 
  dplyr::select(micrositio, ambiente, especie_facilitadora, especie_facilitada, # Categorical var
                altura, diametro_1, diametro_2) |> 
  dplyr::rename(
    Individual  = micrositio,
    Environment = ambiente,
    microsite   = especie_facilitadora, 
    quercus_sp = especie_facilitada, 
    height     = altura, 
    diam_1     = diametro_1, 
    diam_2     = diametro_2
  ) |> 
  mutate(microsite = recode(microsite, "claro" = "gap", "Cistus sp." = "Cistus ladanifer"),
         Environment = recode(Environment, "abierto" = "gap", "pinar" = "pine canopy"),
         Environment = na_if(Environment, "")
  ) |> unique()

### Calculate volume

df.vol <- df.vol |> 
  mutate(
    volume = 4/3 * pi * height/2 * diam_1/2 * diam_2/2
  )

library(readr)
readr::write_csv2(df.vol, "00-data/processed/shrub_volume.csv")
cat("Saved: 00-data/processed/shrub_volume.csv\n")

cat("Shrub size loaded in df.vol \n")

glimpse(df.vol)