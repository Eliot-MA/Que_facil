# 1. load libraries ----
library(dplyr) 
library(tidyr) 
library(rlang)
library(readr)
#library(readxl)
library(ggplot2)
#library(writexl)
library(purrr)



# 2. load your data ----

source("01-scripts/01.1-gps.R")
source("01-scripts/01.2-surv_par_vol.R")


df.surv <- df.surv.par.vol |> 
  select(Individual, zone, microsite, quercus_sp, survival, surv_date) |> 
  mutate(survival = if_else(survival == 2, 1, survival)) |> 
  filter(surv_date == "16/09/2025") |> 
  unique()

df.surv.qf <- df.surv |> 
  filter(quercus_sp == "QF")

df.surv.qi <- df.surv |> 
  filter(quercus_sp == "QI")

#  4. Describe comparisons -----------------------------------
# Each row: focal group (A) vs reference group (B). 

comparisons <- tibble(
  zone_A  = c("pine canopy", "pine canopy",    "pine canopy",
              "open area",   "open area",       "open area",
              "pine canopy"),
  ms_A    = c("Cistus ladanifer",  "Rosa canina",    "Genista scorpius",
              "Cistus ladanifer",  "Rosa canina",    "Genista scorpius",
              "gap"),
  zone_B  = c("pine canopy", "pine canopy",    "pine canopy",
              "open area",   "open area",       "open area",
              "open area"),
  ms_B    = c("gap",         "gap",            "gap",
              "gap",         "gap",            "gap",
              "gap")
)

# 5. Function: cross join + RII per comparison  -----------------

calc_rii <- function(df, zone_A, ms_A, zone_B, ms_B) {
  
  focal <- df |>
    filter(zone == zone_A, microsite == ms_A) |>
    select(ind_A = Individual, zone_A = zone, ms_A = microsite, surv_A = survival)
  
  ref <- df |>
    filter(zone == zone_B, microsite == ms_B) |>
    select(ind_B = Individual, zone_B = zone, ms_B = microsite, surv_B = survival)
  
  cross_join(focal, ref) |>
    mutate(
      RII = if_else(surv_A + surv_B == 0, 0,          # denominador 0 → RII = 0
                    (surv_A - surv_B) / (surv_A + surv_B))
    )
}

#  5. Apply function to every comparison and combine --------------------

df.rii.qi <- pmap(comparisons, calc_rii, df = df.surv.qi) |>
  bind_rows()

df.rii.qf <- pmap(comparisons, calc_rii, df = df.surv.qf) |>
  bind_rows()

#  6. Calculate mean and SE per individual

aggID.df.rii.qi <- df.rii.qi |> 
  group_by(ind_A, zone_A, ms_A) |> 
  summarise(
    mean = mean(RII, na.rm = TRUE), 
    sd   = sd(RII, na.rm = TRUE)
  )

aggID.df.rii.qf <- df.rii.qf |> 
  group_by(ind_A, zone_A, ms_A) |> 
  summarise(
    mean = mean(RII, na.rm = TRUE), 
    sd   = sd(RII, na.rm = TRUE)
  )

theme_set(theme_bw())

ggplot(aggID.df.rii.qf, aes(x = ms_A, y = mean)) + 
  geom_boxplot() +
  geom_violin() +
  geom_jitter() +
  facet_wrap(~zone_A) +
  coord_flip()

p1 <- 
aggID.df.rii.qi |> 
  group_by(ms_A, env_A) |> 
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = media, y = ms_A)) + 
  geom_point() +
  geom_errorbarh(aes(xmin = media - se, xmax = media + se), width = .5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
  facet_wrap(~) +
  ylab("") +
  xlab("RII") +
  labs(title = "Q. ilex") +
  coord_cartesian(xlim = c(-.6, .6))

p2 <-
aggID.df.rii.qf |> 
  group_by(ms_A, zone_A) |> 
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = media, y = ms_A)) + 
  geom_point() +
  geom_errorbarh(aes(xmin = media - se, xmax = media + se), width = .5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
  facet_wrap(~zone_A) +
  ylab("") +
  xlab("RII") +
  labs(title = "Q. faginea") +
  coord_cartesian(xlim = c(-.6, .6))

library(patchwork)

p1/p2

df.rii.qi |> 
  group_by(zone_A, ms_A, zone_B, ms_B) |> 
  summarise(
    n = n()
  )

aggID.df.rii.qi |> 
  filter(ms_A == "Cistus ladanifer") |> 
  summary()

neg.ind <-
aggID.df.rii.qi |> 
  filter(zone_A == "pine canopy") |> 
  filter(mean < 0) |> 
  pull(ind_A)


df.shrubs.gps |> 
  mutate(
    neg.ind = if_else(Individual %in% neg.ind, "neg", "pos")) |> 
  filter(Zone == "pine canopy") |> 
  ggplot(aes(x = Slope, y = neg.ind, colour = factor(neg.ind))) +
  geom_boxplot() +
  geom_jitter()

df.surv.par.vol |> 
  mutate(
    neg_ind = if_else(Individual %in% neg.ind, "neg", "pos")
  ) |> 
  filter(zone == "pine canopy") |> 
  ggplot(aes(x = neg_ind, y = survival)) +
  geom_jitter()

# 4. Create all combinations & calculate the RII 
#(RII is calculated as in line 49: (FOR - GAP) / (FOR + GAP))

big_data <- gap_data |>
  full_join(
    for_data,
    join_by(SITE == SITE, SPECIES == SPECIES),
    relationship = "many-to-many"
  ) |>
  mutate(
    RII = ifelse((FOR + GAP) == 0, 0, (FOR - GAP) / (FOR + GAP))
  )

##what we expect to have : 
#a table of 1800 observations because we have 3 sites x 6 species x 100 possible combinations
#why 100 possibilities: because we have 10 replicates of forest and gaps, each gap can be paired to 10 possible forests


# 5. GAP-CENTERED aggregation: we calculated the mean for each gap with the 10 possibilities of forest
#therefore we obtain at each site and species 10 RII (gap centered) --> a table of 3 sites x 6 species x 10 RII (gap centered) = 180 obs.

agg_data <- big_data |>
  group_by(SITE, CODE_GAP, SPECIES) |>  
  summarise(
    MEAN_RII = mean(RII, na.rm = TRUE),
    SD_RII = sd(RII, na.rm = TRUE),
    .groups = "drop"
  )

# quick check: there should be 10 per SITE × SPECIES (for you it may differ, based on your replicates)
agg_data |>
  count(SITE, SPECIES)


# 6. Mean ± SE across gaps (if only you want to plot so you have 1 RII by treatment combination)
#here we have 3 sites x 6 species = 18 obs.
agg_mean_sd <- agg_data |>
  group_by(SITE, SPECIES) |>
  summarise(
    MEAN = mean(MEAN_RII),
    SE = sd(MEAN_RII) / sqrt(10),
    SUP = MEAN + SE,
    INF = MEAN - SE,
    .groups = "drop"
  ) |>
  mutate(
    SITE = factor(SITE,
                  levels = c("Ehden", "Tannourine", "Shouf"))
  )

# 7. Plot
ggplot(data = agg_mean_sd, aes(x = SITE, y = MEAN)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~SPECIES) +
  geom_errorbar(
    aes(ymin = INF, ymax = SUP),
    width = 0.2
  ) +
  ylim(c(-1, 1)) +
  theme_bw() +
  ylab("Mean RII ± Standard Error (gap-centered)") +
  xlab("Site")

