library(tidyverse)
library(readr)

df.par.ext <- readr::read_csv2("00-data/processed/par_extinction.csv")
df.surv <- readr::read_csv2("00-data/processed/survival.csv")

ggplot(df.par.ext, aes(x = extinction, fill = Environment)) + 
  geom_histogram()  

ind <- df.par.ext$Individual[df.par.ext$extinction < 0]

df.surv |> 
  filter(Individual %in% ind) |> 
  View()
