library(tidyverse)

source("01-scripts/01.1c-PAR_extintion.R")
source("01-scripts/01.2a-survival_growth.R")

ggplot(df.par.ext, aes(x = extinction, fill = Environment)) + 
  geom_histogram()  

ind <- df.par.ext$Individual[df.par.ext$extinction < 0]

df.surv |> 
  filter(Individual %in% ind) |> 
  View()
