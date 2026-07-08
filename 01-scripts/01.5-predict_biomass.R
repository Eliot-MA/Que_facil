# Libraries
library(tidyverse)
library(MuMIn)
library(performance)
library(glmmTMB)

options(na.action = "na.fail")
# Load data
df.biomass <- read.csv2(file = "00-data/shrub_biomass.csv")

source("01-scripts/01.2-surv_par_vol.R")

df.biomass <- 
df.biomass |> 
  rename(volume = Volumen.arbusto..m3., 
         mass = Peso.seco.arbusto..g., 
         log.volume = Log.Volumen, 
         log.mass = Log.masa, 
         zone = ambiente, 
         microsite = Especie) |> 
  mutate(
    microsite = recode(microsite, 
                  "Gs" = "Genista scorpius",
                  "Cl" = "Cistus ladanifer", 
                  "Rc" = "Rosa canina"
                  ),
    zone = recode(zone, 
                  "Abierto" = "open area", 
                  "pinar"   = "pine canopy")
  ) |> 
  mutate(
    zone = factor(zone), 
    microsite = factor(microsite)
  ) 



m_full <- glmmTMB(log.mass ~ log.volume * microsite * zone,
             data = df.biomass)



dd <- dredge(m_full)
modelo <- get.models(dd, subset = 1)[[1]]

# easystats::model_dashboard(modelo, output_dir = "07-html/", output_file = "dashborad_model_biomass.html", browse_html = FALSE, quiet = TRUE)

results_biomass <- df.biomass |>
  group_by(microsite, zone) |>
  nest() |>                                              # crea list-column 'data'
  mutate(
    model  = map(data, ~ glm(mass ~ volume, data = .x, family = Gamma(link = "log"))),
    tidied = map(model, broom::tidy),                   # coeficientes
    glanced = map(model, broom::glance)                 # R², AIC, etc.
  )



# Predict biomass values
df.surv.par.vol <- df.surv.par.vol |> 
  mutate(log.volume = log(volume))

df.model <- 
df.surv.par.vol |> 
  select(Individual, volume, microsite, zone) |> 
  filter(microsite != "gap") |> 
  droplevels() |> 
  drop_na() |> 
  unique()

predictions <- df.model |>
  group_by(microsite, zone) |>
  nest() |>
  left_join(
    results_biomass |> select(microsite, zone, model),
    by = c("microsite", "zone")
  ) |>
  mutate(
    pred.mass = map2(model, data, ~ predict(.x, newdata = .y, type = "response"))
  ) |>
  unnest(c(data, pred.mass))       # devuelve un dataframe plano

# Prediction validation
predictions.validation <- df.biomass |>
  group_by(microsite, zone) |>
  nest() |>
  left_join(
    results_biomass |> select(microsite, zone, model),
    by = c("microsite", "zone")
  ) |>
  mutate(
    pred.mass = map2(model, data, ~ predict(.x, newdata = .y, type = "response"))
  ) |>
  unnest(c(data, pred.mass))       # devuelve un dataframe plano


predictions.validation <- predictions.validation |> 
  mutate(
    res = pred.mass - mass
  )

# 1. Configurar la ventana para tener 1 fila y 2 columnas
par(mfrow = c(1, 2))

# 2. Primer gráfico (Capacidad predictiva)
plot(
  predictions.validation$mass, 
  predictions.validation$pred.mass,
  main = "Predictive capacity of shrub mass.",
  xlab = "Measured mass (g)",
  ylab = "Estimated mass (g)"
)
abline(0, 1, col = "red", lty = 2)

# 3. Segundo gráfico (Homocedasticidad)
plot(
  x = predictions.validation$pred.mass, 
  y = predictions.validation$res, 
  main = "Homoscedasticity of residuals", 
  xlab = "Estimated mass (g)", 
  ylab = "Residuals"
)
abline(h = 0, col = "red", lty = 2)

# 4. (Opcional) Restaurar la ventana gráfica a su estado normal (1 fila, 1 columna)
par(mfrow = c(1, 1))

#RMSE
rmse <- sqrt(mean((predictions.validation$mass - predictions.validation$pred.mass)^2))

# R2 predictivo
predictive.r2 <- 1 - sum((predictions.validation$mass - predictions.validation$pred.mass)^2) /
  sum((predictions.validation$mass - mean(predictions.validation$mass))^2)

# Merge predicted biomass
df.surv.par.vol<- df.surv.par.vol |> 
  left_join(predictions |> select(Individual, pred.mass), 
            by = "Individual") |> 
  select(-zone.y, -microsite.y) |> 
  rename(
    "zone" =     "zone.x",
    "microsite" = "microsite.x"
  )

cat(
  "Biomass estimation script has run successfully. \n",
  "Predictive capacity and homoscedasticity plots are displayed. \n",
  "Root Mean Square Error (RMSE):", rmse, "\n", 
  "Predictive R2:", predictive.r2, "\n", 
  "Estimated shrub mass has been added to the 'df.surv.par.vol' dataset. \n"
)

# performance(m4)
# performance(modelo)
# 
# check_model(modelo)
# 
# car::Anova(m4, type = "II")

# 

# ggplot(df.biomass, 
#        aes(x = log.volume, 
#            y = log.mass, 
#            colour = Especie)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_wrap( ~ambiente)
# 
# df.biomass |> 
#   group_by(ambiente, Especie) |> 
#   summarise(
#     n = n(), 
#     mean_vol = mean(volume),
#     median_vol = median(volume),
#     sd_vol = sd(volume), 
#     mean_mass = mean(mass), 
#     median_mass = median(mass), 
#     sd_mass = sd(mass)
#   )

# INTENTANDO REPLICAR EL RESULTADO DE PEDRO

# options(contrasts = c("contr.sum", "contr.poly"))
# 
# m4_spss <- lm(log.mass ~ log.volume * Especie * ambiente,
#               data = df.biomass)
# 
# car::Anova(m4_spss, type = 3)
# 
# df.biomass.pinar <- df.biomass |>
#   filter(ambiente == "pinar") |>
#   droplevels()
# 
# df.biomass.abierto <- df.biomass |>
#   filter(ambiente == "Abierto") |>
#   droplevels()
# 
# m5.p <- lm(log.mass ~ log.volume*Especie, data = df.biomass.pinar)
# m5.a <- lm(log.mass ~ log.volume*Especie, data = df.biomass.abierto)
# 
# car::Anova(m5.p, type = 3)
