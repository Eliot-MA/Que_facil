library(tidyverse)

source("01-scripts/01.2-surv_par_vol.R")

ggplot(df.ext, aes(x = extinction, fill = zone)) + 
  geom_histogram()  

ind <- df.ext$Individual[df.ext$extinction < 0]

df.surv.par.vol |> 
  filter(Individual %in% ind) |> 
  View()
