# Load data
source("01-scripts/01.1-gps.R")             # gps and elevation info
source("01-scripts/01.2-surv_par_vol.R")    # survival, par and volume
source("01-scripts/01.5-predict_biomass.R") # load biomass

rm(list = setdiff(ls(), c("df.surv.par.vol", "df.shrubs.gps")))

df.shrubs.gps <- df.shrubs.gps |> 
  mutate(
    Elevation_c = scale(Elevation), 
    Individual = as.factor(Individual)
  )

# Merge
df <- merge(
  x = df.shrubs.gps   |> select(Individual, X, Y, Elevation, Elevation_c), 
  y = df.surv.par.vol |> select(Individual, zone, microsite, quercus_sp, height, diam_1, diam_2, volume, pred.mass, survival, surv_date), 
  by = "Individual"
)

# Only data from september

df <- df |> 
  filter(surv_date == "16/09/2025") |> 
  filter(!is.na(survival)) |> 
  unique()

# Dataset with only:
# - survival info
# - size info

df <- df |> 
  filter(microsite != "gap") |> 
  mutate(survival = if_else(survival == 2, 1, survival)) |> 
  drop_na(survival) |> 
  unique()

df <- df |> 
  mutate(
    log.pred.mass   = log(pred.mass), 
    log.pred.mass_c = scale(log.pred.mass)
  )

# Models

# EDA ----
## 1. Matriz de correlaciones (sólo para variables continuas) ----
## Elevation ~ pred.mass
vars <- df[, c("Elevation_c", "pred.mass")]

cor(vars, use = "pairwise.complete.obs")

theme_set(theme_bw())

## 2. Explorar supervivencia frente a cada predictor ----
library(ggplot2)

ggplot(df,
       aes(log(pred.mass), survival)) +
  geom_jitter(height = 0.05, width = 0) +
  geom_smooth(method = "glm",
              method.args = list(family = binomial))

ggplot(df,
       aes(Elevation, survival)) +
  geom_jitter(height = 0.05, width = 0) +
  geom_smooth(method = "glm",
              method.args = list(family = binomial))

ggplot(df,
       aes(microsite, survival)) +
  geom_jitter(height = 0.05, width = 0) +
  geom_boxplot()


# modelos ----
library(glmmTMB)

m_full <- glmmTMB(survival ~ zone*microsite*quercus_sp*Elevation_c*log.pred.mass_c +
    (1 | Individual),
  data = df,
  family = binomial()
  )
  

library(car)

drop1(m_full, test = "Chisq")
# Iteración manual
m2 <- update(m_full, . ~ . - zone:microsite:quercus_sp:Elevation_c:log.pred.mass_c)
drop1(m2, test = "Chisq")

m3 <- update(m2, . ~ . - zone:microsite:Elevation_c:log.pred.mass_c)
drop1(m3, test = "Chisq")

m4 <- update(m3, . ~ . - zone:quercus_sp:Elevation_c:log.pred.mass_c)
drop1(m4, test = "Chisq")

m5 <- update(m4, . ~ . - zone:microsite:quercus_sp:log.pred.mass_c)
drop1(m5, test = "Chisq")

m6 <- update(m5, . ~ . - zone:microsite:log.pred.mass_c)
drop1(m6, test = "Chisq")

m7 <- update(m6, . ~ . - zone:Elevation_c:log.pred.mass_c)
drop1(m7, test = "Chisq")

m8 <- update(m7, . ~ . - microsite:quercus_sp:Elevation_c:log.pred.mass_c)
drop1(m8, test = "Chisq")

m9 <- update(m8, . ~ . - quercus_sp:Elevation_c:log.pred.mass_c)
drop1(m9, test = "Chisq")

m10 <- update(m9, . ~ . - microsite:quercus_sp:log.pred.mass_c)
drop1(m10, test = "Chisq")

m11 <- update(m10, . ~ . - microsite:Elevation_c:log.pred.mass_c)
drop1(m11, test = "Chisq")

m12 <- update(m11, . ~ . - Elevation_c:log.pred.mass_c)
drop1(m12, test = "Chisq")

m13 <- update(m12, . ~ . - zone:quercus_sp:log.pred.mass_c)
drop1(m13, test = "Chisq")

m14 <- update(m13, . ~ . - quercus_sp:log.pred.mass_c)
drop1(m14, test = "Chisq")

# Hay una interaccion de 4v sin interacciones signficiativas de 3 vias por debajo, eliminarla y comparar explicitamente
m15 <- update(m14, . ~ . - zone:microsite:quercus_sp:Elevation_c)
AIC(m14, m15)
drop1(m15, test = "Chisq")

m16 <- update(m15, . ~ . - microsite:quercus_sp:Elevation_c)
drop1(m16, test = "Chisq")

m17 <- update(m16, . ~ . - zone:microsite:quercus_sp)
drop1(m17, test = "Chisq")

m18 <- update(m17, . ~ . - zone:microsite:Elevation_c)
drop1(m18, test = "Chisq")

# Eliminar primero el que más baja el AIC
m19 <- update(m18, . ~ . - microsite:quercus_sp)
drop1(m19, test = "Chisq")

m20 <- update(m19, . ~ . - zone:quercus_sp:Elevation_c)
drop1(m20, test = "Chisq")

m21 <- update(m20, . ~ . - quercus_sp:Elevation_c)
drop1(m21, test = "Chisq")

m22 <- update(m21, . ~ . - zone:quercus_sp)
drop1(m22, test = "Chisq")

# Confirmar que es el mejor de toda la secuencia
AIC(m_full, m22)

# Tabla de efectos
car::Anova(m22)

# Resumen completo
summary(m22)

car::Anova(m22)

modelo <- glmmTMB(survival ~ 
                    zone +
                    microsite +
                    quercus_sp + 
                    Elevation_c + 
                    log.pred.mass_c + 
                    zone:microsite + 
                    zone:Elevation_c + 
                    microsite:Elevation_c +
                    zone:log.pred.mass_c +
                    microsite:log.pred.mass_c +
                    (1 | Individual),
                  data = df,
                  family = binomial()
)

car::Anova(modelo)

# Residuos con DHARMa (diseñado para glmmTMB)
library(DHARMa)
sim_res <- simulateResiduals(m22)
plot(sim_res)
testDispersion(sim_res)

library(performance)

performance(m22)
check_model(m22)
check_collinearity(m22)

library(ggeffects)

# Efecto de zona × microsite
plot(ggpredict(m22, terms = c("zone", "microsite")))

# Efecto de microsite por elevacion
plot(ggpredict(m22, terms = c("Elevation_c[all]", "microsite")))

# Efecto de masa del arbusto por microsite
plot(ggpredict(m22, terms = c("log.pred.mass_c[all]", "microsite")))

# Efecto de masa por zona
plot(ggpredict(m22, terms = c("log.pred.mass_c[all]", "zone")))

# Efecto de elevacion por zona
plot(ggpredict(m22, terms = c("Elevation_c[all]", "zone")))

# Efecto aislado de zona
plot(ggpredict(m22, terms = "zone"))

# Efecto aislado de quercus_sp
plot(ggpredict(m22, terms = "quercus_sp"))

# Graficar proceso selectivo
mods <- list(
  m_full,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,
  m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22
)

aics <- sapply(mods, AIC)

plot(aics, type="b",
     xlab="Iteración",
     ylab="AIC")

npar <- sapply(mods, function(x) attr(logLik(x),"df"))

plot(npar, aics,
     xlab="Número de parámetros",
     ylab="AIC")
text(npar, aics,
     labels=paste0("m", c("full",2:22)),
     pos=3)

library(broom.mixed)
library(tidyverse)

terms_all <- attr(terms(m_full), "term.labels")

presence <- map_dfr(
  seq_along(mods),
  function(i){
    
    tibble(
      model = paste0("m", i),
      term = terms_all,
      present = terms_all %in%
        attr(terms(mods[[i]]), "term.labels")
    )
    
  }
)

ggplot(presence,
       aes(term, model, fill=present))+
  geom_tile()+
  theme_bw()+
  theme(axis.text.x=
          element_text(angle=90,hjust=1)) +
  coord_flip()

delta_aic <- diff(aics)

barplot(delta_aic,
        names.arg=paste0("m",2:22),
        las=2)
abline(h=0,lty=2)
abline(h=c(-2, 2), lty=2)
