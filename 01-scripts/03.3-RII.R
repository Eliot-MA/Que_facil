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
# Bootstrap ----
## 

boot.results <- read.csv2(file = "00-data/boot_RII.csv")

glimpse(boot.results)
summary(boot.results)


# Distribution
boot.results |> 
  filter(surv_date == "27/05/2026") |> 
  ggplot(aes(x = RII)) +
  geom_histogram(aes(fill = quercus_sp), alpha = .5) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  facet_grid(Interacting_species ~ type_RII)


# Diferences from zero
# Actual competition or facilitation
library(ggplot2)

boot.summary <-
  boot.results |>
  group_by(
    surv_date,
    type_RII,
    Interacting_species, 
    quercus_sp
  ) |>
  summarise(
    mean = mean(RII),
    sd = sd(RII),
    se = sd/sqrt(n()),
    IC2.5 = quantile(RII,.025),
    IC97.5 = quantile(RII,.975),
    .groups="drop"
  ) |> 
  mutate(
    contains_zero = if_else(condition = IC2.5 < 0 & IC97.5 < 0 | IC2.5 > 0 & IC97.5 > 0,
                            "no", 
                            "yes"), 
    type_interaction = case_when(
      mean > 0 & contains_zero == "no" ~ "Facilitation",
      mean < 0 & contains_zero == "no" ~ "Competition",
      TRUE ~ "Neutral"
    )
  )

ggplot(data = as.data.frame(boot.summary), 
       aes(x = mean, y = Interacting_species, shape = quercus_sp, colour = type_interaction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = IC2.5, xmax = IC97.5), height = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  facet_grid(type_RII ~ surv_date) +
  theme_bw() +
  coord_cartesian(xlim = c(-.8, .8)) +
  labs(x = "RII", 
       y = "", 
       title = "RII per species, date and type of RII", 
       caption = "RII calculated with bootstrapping (5000 iterations)") 

# Differences between direct and indirect

boot.diff.typeRII <- boot.results |> 
  filter(Interacting_species != "Pinus pinaster") |> 
  group_by(surv_date, Interacting_species, quercus_sp, iteration) |> 
  pivot_wider(
    id_cols = c(surv_date, Interacting_species, quercus_sp, iteration),
    names_from = type_RII, 
    values_from = RII
  ) |> 
  mutate(diff = Direct - Indirect) |>
  ungroup()


ggplot(boot.diff.typeRII, aes(x = diff)) +
  geom_histogram(aes(fill = quercus_sp), alpha = .5) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  facet_grid(Interacting_species ~ surv_date)

boot.indirect.effects <- boot.diff.typeRII |> 
  group_by(
    surv_date, Interacting_species, quercus_sp
  ) |>
  summarise(
    mean = mean(diff),
    sd = sd(diff),
    se = sd/sqrt(n()),
    IC2.5 = quantile(diff,.025),
    IC97.5 = quantile(diff,.975),
    .groups="drop"
  ) |> 
  mutate(
    contains_zero = if_else(condition = IC2.5 < 0 & IC97.5 < 0 | IC2.5 > 0 & IC97.5 > 0,
                            "no", 
                            "yes"), 
    type_interaction = case_when(
      mean > 0 & contains_zero == "no" ~ "Indirect Competition",
      mean < 0 & contains_zero == "no" ~ "Indirect Facilitation",
      TRUE ~ "No Indirect Effect"
    )
  )
  
ggplot(data = as.data.frame(boot.indirect.effects), 
       aes(x = mean, y = Interacting_species, shape = quercus_sp, colour = type_interaction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = IC2.5, xmax = IC97.5), height = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  facet_wrap(~ surv_date) +
  theme_bw() +
  coord_cartesian(xlim = c(-.8, .8)) +
  labs(x = "Difference in RII between gap and Pinus canopy environment", 
       y = "", 
       title = "RII per species, date and type of RII", 
       caption = "RII calculated with bootstrapping (5000 iterations)")

# Differences between quercus species

boot.diff.quercus <- boot.results |> 
  group_by(surv_date, Interacting_species, type_RII, iteration) |> 
  pivot_wider(
    id_cols = c(surv_date, Interacting_species, type_RII, iteration),
    names_from = quercus_sp, 
    values_from = RII
  ) |> 
  mutate(diff = QF - QI) |>
  ungroup()

boot.indirect.effects <- boot.diff.quercus |> 
  group_by(
    surv_date, Interacting_species, type_RII
  ) |>
  summarise(
    mean = mean(diff),
    sd = sd(diff),
    se = sd/sqrt(n()),
    IC2.5 = quantile(diff,.025),
    IC97.5 = quantile(diff,.975),
    .groups="drop"
  ) |> 
  mutate(
    contains_zero = if_else(condition = IC2.5 < 0 & IC97.5 < 0 | IC2.5 > 0 & IC97.5 > 0,
                            "no", 
                            "yes"), 
    type_interaction = case_when(
      mean > 0 & contains_zero == "no" ~ "More effect on Q. faginea",
      mean < 0 & contains_zero == "no" ~ "More effect on Q. ilex",
      TRUE ~ "No differences"
    )
  )



##
# Next approach: 
# RII based on estimated survival probabilities ----
# Survival (0/1)
# │
# V
# Binomial model
# │
# V
# Estimated probabilites
# │
# V
# RII = (pA - pB) / (pA + pB)
## 


