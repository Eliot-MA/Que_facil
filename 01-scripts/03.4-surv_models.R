
# Load surv and quercus volume
source("01-scripts/01.2a-survival_growth.R")

library(tidyverse)

mantener <- c("df.surv", "df.growth", "df.growth.volume")
rm(list = ls()[!(ls() %in% mantener)])


df.surv <- df.surv |> 
  mutate(
    quercus_sp = case_when(
      quercus_sp == "QF" ~ "Quercus faginea", 
      quercus_sp == "QI" ~ "Quercus ilex"
      )
  )

df.growth.volume <- df.growth.volume |> 
  filter(Date == "t1") |> 
  rename(
    Individual = Arbusto, 
    quercus_position = Posición,
    Environment = Ambiente)

df.growth.volume <- df.growth.volume |>
  mutate(
    Individual = str_replace(as.character(Individual),
                             "^([A-Za-z])0+([0-9]+)$",
                             "\\1\\2")
  )

df.growth.volume2 <- df.growth.volume |>
  rename(quercus_sp = Especie)

df.merge <- df.surv |>
  left_join(
    df.growth.volume2 |> select(Individual, quercus_sp, quercus_position, id_quercus, Volume_cm3),
    by = c("Individual", "quercus_sp")
  )

df.merge <- df.merge |> 
  mutate(survival = factor(if_else(condition = survival == 2, 
                            true = 1, 
                            false = survival)))

# Load shrub biomass
source("01-scripts/01.5-predict_biomass.R")

df.shrub.biomass <- df.surv.par.vol |> 
  select(Individual, volume, log.volume, pred.mass) |> 
  rename(shrub_volume = volume, 
         shrub_log_volume = log.volume, 
         shrub_pred_mass = pred.mass)


df.merge2 <- df.merge |> 
  left_join(df.shrub.biomass, by = "Individual")

glimpse(df.merge2)

#---
library(lme4)
library(lmerTest)
df <- df.merge2
df.t2.scaled <- 
  df |> 
  filter(surv_date == "27/05/2026") |> 
  select(-surv_date) |> 
  rename(quercus_volume = Volume_cm3) |> 
  mutate(
    quercus_volume_c = scale(quercus_volume), 
    shrub_volume_c = scale(shrub_volume), 
    shrub_log_volume_c = scale(shrub_log_volume), 
    shrub_pred_mass_c = scale(shrub_pred_mass), 
    shrub_log_pred_mass = log(shrub_pred_mass), 
    shrub_log_pred_mass_c = scale(shrub_log_pred_mass)) |> 
  unique() |> 
  drop_na()


df <- df.t2.scaled

full <- glmmTMB(
  survival ~ quercus_sp * quercus_volume_c * shrub_volume_c * shrub_pred_mass_c * Environment + 
    (1 | Individual),
  data = df,
  family = binomial(), 
  REML = FALSE
)


d <- drop1(full, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
m2 <- update(
  full,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m2
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
m3 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m2
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
m3 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m3
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
m4 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m4
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m5 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m5
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m6 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m6
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m7 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m7
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m8 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m8
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m9 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m9
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m10 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m10
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m11 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m11
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m12 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m12
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m13 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m13
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m14 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m14
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m15 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m15
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m16 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m16
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m17 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m17
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m18 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m18
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m19 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m19
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m20 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m20
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m21 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m21
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef
m22 <- update(
  modelo,
  as.formula(paste(". ~ . -", drop.coef))
)

modelo <- m22
d <- drop1(modelo, test = "Chisq")
d
drop.coef <- row.names(d)[which.min(d$AIC)]
drop.coef

mfinal <- update(m22, REML = TRUE)

mfinal <- glmmTMB(survival ~ quercus_sp + quercus_volume_c + shrub_volume_c + shrub_pred_mass_c + 
                    Environment + (1 | Individual) + quercus_sp:quercus_volume_c + 
                    shrub_volume_c:shrub_pred_mass_c + shrub_volume_c:Environment + 
                    shrub_pred_mass_c:Environment + shrub_volume_c:shrub_pred_mass_c:Environment, 
                  data = df,
                  family = binomial(), 
                  REML = TRUE
)

#---

car::Anova(mfinal)

library(sjPlot)

plot_model(mfinal, type = "pred", terms = "Environment")
plot_model(mfinal, type = "pred", terms = c("quercus_sp", "quercus_volume_c"))
plot_model(mfinal, type = "pred", terms = c("shrub_volume_c", "shrub_pred_mass_c", "Environment"))

library(MASS)
stepAIC(full)

modelo_final <- stepAIC(
  full,
  direction = "backward"
)

#==
full <- glmmTMB(
  survival ~ quercus_sp * quercus_volume_c * shrub_volume_c * Environment + 
    (1 | Individual),
  data = df,
  family = binomial(), 
  REML = FALSE
)

library(MuMIn)

options(na.action = "na.fail")

library(parallel)
parallel::detectCores()

cl <- makeCluster(detectCores() - 1)

dd <- dredge(
  full,
  cluster = cl
)

dd <- dredge(
  full,
  component = "cond",
  cluster = cl
)

dd <- dredge(
  full,
  fixed = c("(Intercept)"),
  cluster = cl
)
