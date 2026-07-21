## Shrubs size
df.surv.par.vol <- read.csv2("00-data/vol_par_surv.csv")

df.vol <- df.surv.par.vol |> 
  dplyr::select(micrositio, ambiente, especie_facilitadora, especie_facilitada, # Categorical var
                altura, diametro_1, diametro_2) |> 
  dplyr::rename(
    Individual = micrositio,
    zone       = ambiente,
    microsite  = especie_facilitadora, 
    quercus_sp = especie_facilitada, 
    height     = altura, 
    diam_1     = diametro_1, 
    diam_2     = diametro_2
  ) |> 
  mutate(microsite = recode(microsite, "claro" = "gap", "Cistus sp." = "Cistus ladanifer"),
         zone      = recode(zone, "abierto" = "open area", "pinar" = "pine canopy")
  ) |> unique()

### Calculate volume

df.vol <- df.vol |> 
  mutate(
    volume = 4/3 * pi * height/2 * diam_1/2 * diam_2/2
  )

cat("Shrub size loaded in df.vol \n")

glimpse(df.vol)