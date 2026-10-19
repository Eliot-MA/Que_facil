# Working on it ...

cat("Loading specific leaf area data... \n")

rD.sla <- read.csv2("00-data/raw/shrub_SLA.csv")

library(tidyverse)

df.sla <- rD.sla |> 
  select(ID, especie_facilitadora, Ambiente, peso_seco, area_total..cm2., SLA..cm2.g.) |> 
  rename(Individual = ID, 
         shrub = especie_facilitadora, 
         Environment = Ambiente, 
         leaf_dry_weight = peso_seco, 
         leaf_area = area_total..cm2., 
         SLA = SLA..cm2.g.)

cat("rD.sla contains the original dataframe \n")
glimpse(rD.sla)

cat("df.sla contains a selection of the original columns and translated column names \n")
glimpse(df.sla)


