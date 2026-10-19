# Supplementary analysis: RII on survival data
# This script depends on objects created in:
source("01-scripts/01.2a-survival_growth.R")

library(tidyverse)
library(glmmTMB)

# 1. Prepare data ----

# Select variables of interest
df.surv <- df.surv |>
  select(Individual, Environment, microsite, quercus_sp, survival, surv_date) |>
  mutate(survival = if_else(survival == 2, 1, survival)) |>
  filter(surv_date != "") # J_53bis aparece con campo vacío en fecha

# Create subdatasets per quercus species
df.surv.qf <- df.surv |>
  filter(quercus_sp == "QF")

df.surv.qi <- df.surv |>
  filter(quercus_sp == "QI")

# 2. Describe comparisons ----
# Each row: focal group (A) vs reference group (B).

comparisons <- tibble(
  env_A  = c("pine canopy", "pine canopy",    "pine canopy",
             "gap",   "gap",       "gap",
             "pine canopy"),
  ms_A    = c("Cistus ladanifer",  "Rosa canina",    "Genista scorpius",
              "Cistus ladanifer",  "Rosa canina",    "Genista scorpius",
              "open"),
  env_B  = c("pine canopy", "pine canopy",    "pine canopy",
             "gap",   "gap",       "gap",
             "gap"),
  ms_B    = c("open",         "open",            "open",
              "open",         "open",            "open",
              "open"),
  type_RII = c("Indirect", "Indirect", "Indirect",
               "Direct",   "Direct",   "Direct",
               "Direct"),
  Interacting_species = c("Cistus ladanifer", "Rosa canina", "Genista scorpius",
                          "Cistus ladanifer", "Rosa canina", "Genista scorpius",
                          "Pinus pinaster")
)

# 3. Classic RII (empirical, on individual survival) ----

calc_rii <- function(df, env_A, ms_A, env_B, ms_B, type_RII, Interacting_species) {

  # Create focal group
  focal <- df |>
    filter(Environment == env_A, microsite == ms_A) |>
    select(
      ind_A = Individual,
      env_A = Environment,
      ms_A = microsite,
      surv_A = survival,
      surv_date
    )

  # Create reference group
  ref <- df |>
    filter(Environment == env_B, microsite == ms_B) |>
    select(
      ind_B = Individual,
      env_B = Environment,
      ms_B = microsite,
      surv_B = survival,
      surv_date
    )

  # Inner join allow to make comparison only within the same date
  inner_join(focal, ref, by = "surv_date") |>
    mutate(
      type_RII = type_RII,
      Interacting_species = Interacting_species,
      RII = if_else(
        surv_A + surv_B == 0,
        0,
        (surv_A - surv_B) / (surv_A + surv_B)
      )
    )
}

df.rii.qi <- pmap(comparisons, calc_rii, df = df.surv.qi) |> bind_rows()
df.rii.qf <- pmap(comparisons, calc_rii, df = df.surv.qf) |> bind_rows()

# Aggregated RII per individual
aggID.df.rii.qi <- df.rii.qi |>
  group_by(ind_A, surv_date, type_RII, Interacting_species) |>
  summarise(
    mean = mean(RII, na.rm = TRUE),
    sd   = sd(RII, na.rm = TRUE),
    .groups = "drop"
  )

aggID.df.rii.qf <- df.rii.qf |>
  group_by(ind_A, surv_date, type_RII, Interacting_species) |>
  summarise(
    mean = mean(RII, na.rm = TRUE),
    sd   = sd(RII, na.rm = TRUE),
    .groups = "drop"
  )

# Population-level aggregate (first census excluded, as in the original analysis)
agg.rii.qi <- aggID.df.rii.qi |>
  filter(surv_date != "12/06/2025") |>
  group_by(surv_date, type_RII, Interacting_species) |>
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |>
  mutate(quercus_sp = "Q. ilex")

agg.rii.qf <- aggID.df.rii.qf |>
  filter(surv_date != "12/06/2025") |>
  group_by(surv_date, type_RII, Interacting_species) |>
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |>
  mutate(quercus_sp = "Q. faginea")

agg.rii <- rbind(agg.rii.qi, agg.rii.qf)

# Classic RII figure
p_classic <- ggplot(agg.rii, aes(x = media, y = Interacting_species, colour = quercus_sp)) +
  geom_point(position = position_dodge(width = .5), size = 1.5) +
  geom_errorbarh(aes(xmin = media - se, xmax = media + se), width = .5, position = position_dodge(width = .5)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
  facet_grid(surv_date ~ type_RII) +
  ylab("") +
  xlab("RII") +
  labs(title = "Direct and indirect RII (classic, empirical)") +
  coord_cartesian(xlim = c(-.6, .6)) +
  theme_light() +
  theme(legend.position = "bottom")

ggsave("08-img/RII_classic.png", plot = p_classic, width = 10, height = 8, dpi = 300)

# 4. RII from a GLMM on survival (comparison) ----
# A single binomial GLMM is fitted per census date; RII is then computed from
# the predicted survival probabilities. This is the reference approach the RII
# is meant to be compared against.

pred_grid <- df.surv |>
  select(Environment, microsite, quercus_sp) |>
  distinct() |>
  expand_grid(surv_date = unique(df.surv$surv_date)) |>
  mutate(Individual = "dummy")

calc_rii_prob <- function(df_prob, env_A, ms_A, env_B, ms_B,
                          type_RII, Interacting_species) {

  focal <- df_prob |>
    filter(Environment == env_A, microsite == ms_A) |>
    select(quercus_sp, surv_date, p_A = survival)

  ref <- df_prob |>
    filter(Environment == env_B, microsite == ms_B) |>
    select(quercus_sp, surv_date, p_B = survival)

  inner_join(focal, ref, by = c("quercus_sp", "surv_date")) |>
    mutate(
      type_RII           = type_RII,
      Interacting_species = Interacting_species,
      # For binary data the RII collapses to a difference of survival
      # probabilities, p_A - p_B (see RII_survival.qmd), so the GLMM-based
      # index is computed on the same scale to make the two comparable.
      RII = p_A - p_B
    ) |>
    select(surv_date, quercus_sp, type_RII, Interacting_species, RII)
}

fit_glmm_rii <- function(d) {

  df_d <- df.surv |> filter(surv_date == d)

  m <- tryCatch(
    glmmTMB(
      survival ~ quercus_sp + Environment * microsite + (1 | Individual),
      data = df_d,
      family = binomial()
    ),
    error = function(e) NULL,
    warning = function(e) NULL
  )

  if (is.null(m)) return(NULL)

  prob_d <- pred_grid |>
    filter(surv_date == d) |>
    mutate(survival = predict(m, newdata = pred_grid |> filter(surv_date == d),
                              type = "response", allow.new.levels = TRUE))

  pmap(comparisons, calc_rii_prob, df_prob = prob_d) |>
    bind_rows() |>
    mutate(quercus_sp = recode(quercus_sp, QI = "Q. ilex", QF = "Q. faginea"))
}

model_rii <- map_dfr(unique(df.surv$surv_date), fit_glmm_rii)

# 5. Compare classic vs GLMM-based RII ----

cmp <- agg.rii |>
  left_join(
    model_rii,
    by = c("surv_date", "type_RII", "Interacting_species", "quercus_sp")
  ) |>
  rename(RII_classic = media, RII_glmm = RII)

p_cmp <- ggplot(cmp, aes(x = RII_classic, y = RII_glmm, colour = quercus_sp)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 1.5) +
  facet_grid(surv_date ~ type_RII) +
  coord_cartesian(xlim = c(-.6, .6), ylim = c(-.6, .6)) +
  labs(
    x = "RII (classic, empirical)",
    y = "RII (GLMM on survival)",
    title = "Classic RII vs GLMM-based RII",
    subtitle = "Points on the dashed line indicate agreement between methods"
  ) +
  theme_light() +
  theme(legend.position = "bottom")

ggsave("08-img/RII_classic_vs_glmm.png", plot = p_cmp, width = 10, height = 8, dpi = 300)

write.csv2(cmp, file = "00-data/processed/RII_classic_vs_glmm.csv", row.names = FALSE)
