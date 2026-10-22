library(tidyverse)
library(stringr)
library(hms)

df.surv.par.vol <- read.csv2("00-data/raw/vol_par_surv.csv")


df.par <- df.surv.par.vol |> 
  dplyr::select(micrositio, # ID 
                ambiente, especie_facilitadora, # Categorical var
                luminosidad_1, luminosidad_2, luminosidad_3, fecha_luminosidad, posicion_luminosidad, # abiotic, PAR, 
  ) |> 
  dplyr::rename(
    Individual   = micrositio,
    Environment  = ambiente,
    microsite    = especie_facilitadora, 
    par_1        = luminosidad_1, 
    par_2        = luminosidad_2, 
    par_3        = luminosidad_3, 
    par_date     = fecha_luminosidad, 
    par_posi     = posicion_luminosidad
  ) |> 
  mutate(microsite = recode(microsite, "claro" = "open", "Cistus sp." = "Cistus ladanifer"),
         Environment = recode(Environment, "abierto" = "gap", "pinar" = "pine canopy"),
         Environment = na_if(Environment, "")
  ) |> 
  unique()

##########################
# Compute par extinction #----
##########################

# 1. Open area ----

df.par <- df.par |> 
  pivot_longer(cols = c(par_1, par_2, par_3) , names_to = "replicate_par", values_to = "par") |> 
  mutate(replicate_par = as.factor(substring(replicate_par, 5, 5)))

df.par <- df.par |> 
  group_by(Individual, par_posi) |> 
  summarise(
    n = n(), 
    Environment = first(Environment), 
    microsite = first(microsite),
    par = mean(par)) |> 
  ungroup()

# Merge hour measurment data
rD.time.open <- read.csv2("00-data/raw/par_shrubs_time_open.csv", 
                          fileEncoding = "latin1")

rD.time.pine <- read.csv2("00-data/raw/par_shrubs_time_pine.csv", 
                          fileEncoding = "latin1")

df.time.open <- rD.time.open |> mutate(
  micrositio_posición = NA, .before = micrositio
) |> 
  mutate(
    posicion_luminosidad = NA, .before = hora_luminosidad
  )

df.time <- rbind(df.time.open, rD.time.pine)

df.time <- df.time |> 
  filter(!str_detect(micrositio, "Promedio"))

df.time <- df.time |> 
  select(micrositio, hora_luminosidad) |> 
  unique()

df.par <- merge(x = df.par, 
                y = df.time, 
                by.x = "Individual", 
                by.y = "micrositio")

df.par.gap <- df.par |> filter(Environment == "gap")
df.par.pine <- df.par |> filter(Environment == "pine canopy")


## 3. Compute absolute time difference ----

### 1) Convertir la hora a formato temporal ----
df.par.gap <- df.par.gap %>%
  mutate(hora_luminosidad = as_hms(hora_luminosidad))

### 2) Separar no-gap y gap ----
df.shrub.open <- df.par.gap %>%
  filter(microsite != "open") %>%
  transmute(
    Individual_shrub = Individual,
    microsite_shrub  = microsite,
    hora_shrub       = hora_luminosidad
  )

df.gap.open <- df.par.gap %>%
  filter(microsite == "open") %>%
  transmute(
    Individual_gap = Individual,
    microsite_gap  = microsite,
    hora_gap       = hora_luminosidad
  )

### 3) Cruce completo y diferencia absoluta en minutos ----
df.dif.hora.open <- crossing(df.shrub.open, df.gap.open) %>%
  mutate(
    diff_min = abs(as.numeric(hora_shrub - hora_gap)) / 60
  )

## 4. Select control gaps ----
df.dif.hora.open.min <- df.dif.hora.open %>%
  group_by(Individual_shrub) %>%
  slice_min(order_by = diff_min, n = 1, with_ties = TRUE) %>%
  ungroup()

## 5. Compute gap controls ----

### 1) merge gaps par ----
df <- df.par.gap |> select(Individual,par)
df.dif.hora.open.min.par <- df.dif.hora.open.min |> 
  left_join(df, by = c("Individual_gap" = "Individual"))

### 2) create par control table ----
df.gap.shrub.par.control <- 
  df.dif.hora.open.min.par %>%
  group_by(Individual_shrub) %>%
  summarise(
    Individual_gap = paste(sort(unique(Individual_gap)), collapse = "-"),
    par_gap = mean(par, na.rm = TRUE),
    .groups = "drop"
  )

## 6. Compute par extinction ----
df.par.gap.ext <- df.par.gap |> 
  filter(microsite != "gap") |> 
  left_join(df.gap.shrub.par.control, 
            by = c("Individual" = "Individual_shrub"))

df.par.gap.ext <- df.par.gap.ext |> 
  mutate(
    extinction = ((par_gap - par) / par_gap) * 100
  )

# 2. Pine canopy ----

## 1) Pivot wider position par ----

df.par.pine.ext <- df.par.pine |> 
  filter(microsite != "open", Individual != "J56") |> 
  pivot_wider(names_from = par_posi, values_from = par) |> 
  mutate(
    extinction = ((encima - debajo) / (encima)) * 100
  )

# 3. Create dataframe with only extinction ----
df.ext <-
  rbind(
    df.par.gap.ext |> select(Individual, Environment, microsite, hora_luminosidad, extinction),
    df.par.pine.ext |> select(Individual, Environment, microsite, hora_luminosidad, extinction)
  )

# 4. Include extintion in the original dataframe ----
# Extinction is a single value per individual. df.par is long over par_posi, so
# that value necessarily repeats for every par_posi row of the individual.
# The join is keyed by (Individual, par_posi) to keep one row per measurement
# and avoid the many-to-many crossing caused by individuals with >1 par_posi.
df.ext.ind <- df.ext |> 
  group_by(Individual) |> 
  summarise(
    extinction = mean(extinction, na.rm = TRUE), 
    .groups = "drop"
  ) |> 
  mutate(extinction = if_else(is.nan(extinction), NA_real_, extinction))

df.ext.pos <- df.par |> 
  distinct(Individual, par_posi) |> 
  left_join(df.ext.ind, by = "Individual")

df.par.ext <- df.par |> 
  left_join(df.ext.pos, by = c("Individual", "par_posi"))

df.par.ext <- df.par.ext |> 
  mutate(extinction = if_else(microsite == "open", 0, extinction)) 

# 5. Check errors ----

# ind <- df.ext$Individual[df.ext$extinction < 0]
# 
# df.par.pine.ext |> 
#   filter(Individual %in% ind) |> 
#   View()



# rm("df", 
#    "df.dif.hora.open", 
#    "df.dif.hora.open.min", 
#    "df.dif.hora.open.min.par", 
#    "df.gap.open", 
#    "df.gap.shrub.par.control", 
#    "df.par", 
#    "df.par.gap", 
#    "df.par.gap.ext", 
#    "df.par.pine", 
#    "df.par.pine.ext", 
#    "df.shrub.open", 
#    "df.time", 
#    "df.time.open", 
#    "rD.time.open", 
#    "rD.time.pine")

cat("PAR loaded and extintion calculated.\n")

print(head(df.par.ext, 10))

