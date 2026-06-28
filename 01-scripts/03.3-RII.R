library(tidyverse)
library(broom)

source("01-scripts/01.21-survival.R")
source("01-scripts/01.22-RII.R")

# Añadir nombre de especie
rii.qi <- aggID.df.rii.qi |>
  mutate(quercus_sp = "Q. ilex")

rii.qf <- aggID.df.rii.qf |>
  mutate(quercus_sp = "Q. faginea")

rii.all <- bind_rows(rii.qi, rii.qf) |>
  # For simplicity, first date survival is excluded, 
  # because of the absence of differences
  filter(surv_date != "12/06/2025") 

# One-sample t-tests against RII = 0

ttest.rii <- rii.all |>
  group_by(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species
  ) |>
  summarise(
    test = list(t.test(mean, mu = 0)),
    .groups = "drop"
  ) |>
  mutate(tidy = map(test, broom::tidy)) |>
  unnest(tidy) |>
  select(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species,
    estimate,
    statistic,
    parameter,
    conf.low,
    conf.high,
    p.value
  ) |>
  arrange(quercus_sp,
          surv_date,
          type_RII,
          Interacting_species)

# ttest.rii

ttest.rii <- ttest.rii |>
  mutate(
    interpretation = case_when(
      p.value < 0.05 & estimate > 0 ~ "Facilitation",
      p.value < 0.05 & estimate < 0 ~ "Competition",
      TRUE ~ "Neutral"
    )
  )

p <- 
ggplot(
  ttest.rii,
  aes(
    x = estimate,
    y = Interacting_species,
    colour = interpretation
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(shape = quercus_sp), size = 3, 
             position = position_dodge(width = 1)) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high, 
      shape = quercus_sp
    ),
    height = .2, 
    position = position_dodge(width = 1)
  ) +
  facet_grid(
    surv_date ~  type_RII
  ) +
  theme_light() +
  labs(
    x = "Mean RII",
    y = "",
    colour = "Effect"
  )

ggsave("08-img/RII_ttest.png", plot = p, width = 10, height = 8, dpi = 300)

# Check model

library(tidyverse)

normality.tests <-
  rii.all |>
  group_by(
    quercus_sp,
    surv_date,
    type_RII,
    Interacting_species
  ) |>
  summarise(
    p.shapiro =
      shapiro.test(mean)$p.value,
    .groups = "drop"
  )

# normality.tests 
# Absence of normality in all cases
p <- 
ggplot(aggID.df.rii.qi |> filter(surv_date == "27/05/2026"), 
       aes(x = mean)) +
  geom_histogram(bins = 30) +
  #geom_density() +
  coord_cartesian(xlim = c(-1, 1)) +
  facet_grid(Interacting_species~type_RII) +
  labs(
    title = "RII distribution in may-2026 date",
    subtitle = "RII calculated with survival data (0/1)",
    caption = "In this graph we can observe that the calculation of the RII, \n 
    based on binomial data, and following the method describe in the results report 2, \n 
    produce a binomial distribution. Initially, we expected to generate a normal distribution."
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, face = "italic", color = "gray30")
  )

ggsave("08-img/RII_binomial_distribution.png", plot = p, width = 10, height = 8, dpi = 300)

##
# Next approach: 
# RII based on estimated survival probabilities ----
# Survival (0/1)
# │
# V
# Binomial model
# │
# ▼
# Estimated probabilites
# │
# ▼
# RII = (pA - pB) / (pA + pB)
## 


