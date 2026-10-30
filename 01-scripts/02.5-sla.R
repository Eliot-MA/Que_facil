library(tidyverse)
library(readr)

df.sla <- readr::read_csv2("00-data/processed/sla.csv")

# Exploratory analysis ----
glimpse(df.sla)
print(summary(df.sla))
