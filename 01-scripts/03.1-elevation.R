library(tidyverse)
library(easystats)

# Load data
source("01-scripts/01.1-gps.R")          # gps and elevation info
source("01-scripts/01.2-surv_par_vol.R") # survival, par and volume

n.values <- c(2, 3)

# Variables indicating elevation range
q.elevation1 <- quantile(
  df.shrubs.gps$Elevation,
  probs = seq(0, 1, by = 1/n.values[1])
)[-c(1, n.values[1]+1)]

q.elevation2 <- quantile(
  df.shrubs.gps$Elevation,
  probs = seq(0, 1, by = 1/n.values[2])
)[-c(1, n.values[2]+1)]

threshold1 <- as.numeric(q.elevation1)
threshold2 <- as.numeric(q.elevation2)

df.shrubs.gps <- df.shrubs.gps |>
  mutate(
    ElevationClass1 = case_when(
      Elevation < threshold1 ~ "low",
      Elevation >= threshold1 ~ "high"
    ), 
    ElevationClass2 = case_when(
      Elevation < threshold2[1] ~ "low", 
      Elevation >= threshold2[1] & Elevation < threshold2[2] ~ "medium", 
      Elevation >= threshold2[2] ~ "high"
    ), 
    Elevation_c = scale(Elevation), 
    Individual = as.factor(Individual)
  )

# Explore another kind of division

x.quantiles <- quantile(df.shrubs.gps$X, probs = seq(0, 1, by = 1/6))
x.threshold <- as.numeric(x.quantiles)

ggplot(df.shrubs.gps, aes(x = X, y = Y, colour = microsite_code)) +
  geom_point() +
  geom_vline(xintercept = x.quantiles)

df.shrubs.gps <- df.shrubs.gps |>
  mutate(
    X.class = cut(
      X,
      breaks = quantile(X, probs = seq(0, 1, by = 1/6), na.rm = TRUE),
      labels = paste0("c", 1:6),
      include.lowest = TRUE
    )
  )

ggplot(df.shrubs.gps, aes(x = X, y = Y, colour = X.class)) +
  geom_point() +
  geom_vline(xintercept = x.quantiles)

table(df.shrubs.gps$microsite_code, df.shrubs.gps$X.class)

# Number of observation per treatment per elevation class

# merge gps and survival

df <- merge(
  x = df.shrubs.gps   |> select(Individual, X, Y, Elevation, ElevationClass1, ElevationClass2, Elevation_c), 
  y = df.surv.par.vol |> select(Individual, zone, microsite, quercus_sp, height, diam_1, diam_2, survival, surv_date), 
  by = "Individual"
)

# Only data from september

df <- df |> 
  filter(surv_date == "16/09/2025") |> 
  filter(!is.na(survival)) |> 
  unique()


library(waffle)
waffle.chart <- 
ggplot(data = df |> 
  group_by(zone, microsite, ElevationClass1) |> 
  summarise(
    n = n()
  ), aes(fill = microsite, values = n)) +
  geom_waffle(color = "white", size = 0.8, n_rows = 6) +
  facet_grid(ElevationClass1~zone) +
  theme_void() +
  scale_fill_manual(values = c("#69b3a2", "#404080", "#FFA07A", "#FFD700", "#FF6347", "#4682B4"))

ggsave(
  filename = "08-img/waffle_zone_microsite_ElevatioClass1.png",
  plot = waffle.chart,
  width = 8,
  height = 10,
  dpi = 300
)

## Accute unbalance between groups



  

# Graficos
species <- unique(df$microsite)

for (i in 1:length(species)) {
  p <-
  df |> 
    filter(microsite == species[i]) |> 
  ggplot(aes(x = Elevation, y = factor(survival))) +
    geom_boxplot() +
    geom_violin(alpha = 0.2) +
    geom_jitter() +
    facet_grid(zone~microsite) +
    labs(title = species[i])
  print(p)
  ggsave(
    filename = paste0("08-img/boxplot_elevation_survial_", 
                      species[i], 
                      "_.png"),
    plot = waffle.chart,
    width = 8,
    height = 10,
    dpi = 300
  )
}

# No significant differences between class 2 and 1 
# of survival -> they can be merged in only one class
df <- df |>
  mutate(
    survival = if_else(survival == 2, 1, survival)
  )


library(glmmTMB)
library(performance)
# ¿Best model to account for elevation effect?

m1 <- glmmTMB(survival ~ Elevation_c +
                zone * microsite * quercus_sp +
                (1|Individual), 
              data = df, 
              family = binomial(link = "logit"))

m2 <- glmmTMB(survival ~ ElevationClass1 +
                zone * microsite * quercus_sp +
                (1|Individual), 
              data = df, 
              family = binomial(link = "logit"))

m3 <- glmmTMB(survival ~ ElevationClass2 + 
                zone * microsite * quercus_sp +
                (1|Individual), 
              data = df, 
              family = binomial(link = "logit"))


compare_performance(m1, m2, m3)

# Is the relation lineal or not lineal?

library(mgcv)

gam <- gam(
  survival ~ s(Elevation_c) +
    zone * microsite * quercus_sp +
    s(Individual, bs = "re"),
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

glm <- gam(
  survival ~ Elevation_c +
    zone * microsite * quercus_sp +
    s(Individual, bs = "re"),
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

glm2 <- gam(
  survival ~ poly(Elevation_c, degree = 2) +
    zone * microsite * quercus_sp +
    s(Individual, bs = "re"),
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)


summary(gam1)
plot(gam1)
AIC(glm, glm2, gam)

# There is no strong evidence of linearity. 
# Smooth term (edf = 1.54; p = 0.239)

library(easystats)
library(glmmTMB)

# Does elevation have an effect?

m_full <- glmmTMB(survival ~ quercus_sp + Elevation_c*microsite*zone +
                    (1|Individual), data = df, family = binomial(link = "logit"))

parameters(m_full)
check_collinearity(m_full)

# library(MuMIn)
# options(na.action = "na.fail")
# 
# dd <- dredge(m_full)
# 
# glmm1 <- get.models(dd, subset = 1)[[1]]
# glmm2 <- get.models(dd, subset = 2)[[1]]
# glmm3 <- get.models(dd, subset = 3)[[1]]
# 
# compare_performance(glmm1, glmm2, glmm3)
# 
# formula(glmm1)
# formula(glmm2)
# formula(glmm3)
# 
# car::Anova(glmm1)
# car::Anova(glmm2)
# car::Anova(glmm3)
# 
# parameters(glmm1)
# parameters(glmm2)
# parameters(glmm3)

library(stats)

m_final <- step(m_full, direction = "backward")


# Residuos
library(DHARMa)

res <- simulateResiduals(glmm1, n = 999)

testOverdispersion(res)

# Visualize models
library(sjPlot)

p1 <- 
plot_model(
  glmm1,
  type = "pred",
  terms = c("Elevation_c[all]", "microsite"), 
  show.data = TRUE
)

ggsave(
  filename = "08-img/pred_elevation_microsite.png",
  plot = p1,
  width = 8,
  height = 10,
  dpi = 300
)

p2 <-
plot_model(
  glmm1,
  type = "pred",
  terms = c("microsite", "zone")
) + coord_flip()

ggsave(
  filename = "08-img/pred_microsite_zone.png",
  plot = p2,
  width = 8,
  height = 10,
  dpi = 300
)

p3 <- plot_model(
  glmm1,
  type = "pred",
  terms = "quercus_sp"
) + coord_flip()

ggsave(
  filename = "08-img/pred_quercussp.png",
  plot = p3,
  width = 8,
  height = 10,
  dpi = 300
)


p4 <- 
plot_model(
  glmm2, 
  type = "pred", 
  terms = c("Elevation_c[all]", "zone"), 
  show.data = TRUE
)

ggsave(
  filename = "08-img/pred_elevation_zone.png",
  plot = p4,
  width = 8,
  height = 10,
  dpi = 300
)



