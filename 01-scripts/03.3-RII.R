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

boot.results <- read.csv2("00-data/boot_RII.csv")

boot.results <- boot.results |> 
  rename(Environment = type_RII) |> 
  mutate(Environment = case_when(
    Environment == "Direct" ~ "simple effect", 
    Environment == "Indirect" ~ "conditioned effect"
  ))
# Florian noted that terms Direct and Indirect are the result of inference
# it's more appropiate to name this variable as the environment 
# where the interaction occure

library(dplyr)
library(tidyr)


## Bootstrap summary functions ----


interpret_boot <- function(mean, IC2.5, IC97.5,
                           positive_label,
                           negative_label,
                           neutral_label){
  
  contains_zero <- dplyr::between(0, IC2.5, IC97.5)
  
  interpretation <-
    dplyr::case_when(
      contains_zero ~ neutral_label,
      mean > 0      ~ positive_label,
      TRUE          ~ negative_label
    )
  
  p_boot <- 2 * min(mean <= 0, mean >= 0)
  
  tibble(
    contains_zero = contains_zero,
    p_boot = p_boot,
    interpretation = interpretation
  )
}


summarise_boot <- function(df,
                           value,
                           positive_label,
                           negative_label,
                           neutral_label){
  
  value <- rlang::ensym(value)
  
  out <-
    df |>
    summarise(
      bootstrap_mean = mean(!!value),
      bootstrap_median = median(!!value),
      bootstrap_sd = sd(!!value),
      IC2.5 = quantile(!!value, .025),
      IC97.5 = quantile(!!value, .975),
      .groups = "drop"
    )
  
  bind_cols(
    out,
    interpret_boot(
      out$bootstrap_mean,
      out$IC2.5,
      out$IC97.5,
      positive_label,
      negative_label,
      neutral_label
    )
  )
}

## 1. Is RII different from zero? ----
  boot.zero <-
  
  boot.results |>
  
  group_by(
    surv_date,
    Environment,
    Interacting_species,
    quercus_sp
  ) |>
  
  group_modify(~{
    
    summarise_boot(
      .x,
      RII,
      positive_label = "Facilitation",
      negative_label = "Competition",
      neutral_label  = "Neutral"
    )
    
  }) |>
  
  ungroup()

pd <- .5
ggplot(boot.zero, aes(x = bootstrap_mean, y = Interacting_species, shape = quercus_sp, colour = interpretation)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(position = position_dodge(width = pd)) +
  geom_errorbar(aes(xmin = IC2.5, xmax = IC97.5), width = .5, position = position_dodge(width = pd)) +
  coord_cartesian(xlim = c(-1, 1)) +
  facet_grid(Environment ~ surv_date)

## 2. Direct vs Indirect ----
boot.diff.env <-
  
  boot.results |>
  
  filter(Interacting_species != "Pinus pinaster") |>
  
  pivot_wider(
    
    id_cols = c(
      surv_date,
      Interacting_species,
      quercus_sp,
      iteration
    ),
    
    names_from = Environment,
    values_from = RII
    
  ) |>
  
  mutate(
    diff = `simple effect` - `conditioned effect`
  )

boot.env.effect <-
  
  boot.diff.env |>
  
  group_by(
    surv_date,
    Interacting_species,
    quercus_sp
  ) |>
  
  group_modify(~{
    
    summarise_boot(
      .x,
      diff,
      positive_label = "Indirect competition",
      negative_label = "Indirect facilitation",
      neutral_label  = "No difference"
    )
    
  }) |>
  
  ungroup()

pd <- .5
ggplot(boot.env.effect, aes(x = bootstrap_mean, y = Interacting_species, shape = quercus_sp, colour = interpretation)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(position = position_dodge(width = pd)) +
  geom_errorbar(aes(xmin = IC2.5, xmax = IC97.5), width = .5, position = position_dodge(width = pd)) +
  #coord_cartesian(xlim = c(-1, 1)) +
  facet_wrap(~ surv_date) +
  labs(title = "Mean differences in RII between gap and canopy", 
       caption = "Positive differences indicate indirect competition \n
       Negative differences indicate indirect facilitation")

## 3. Q. ilex vs Q. faginea ----
boot.diff.quercus <-
  
  boot.results |>
  
  pivot_wider(
    
    id_cols = c(
      surv_date,
      Environment,
      Interacting_species,
      iteration
    ),
    
    names_from = quercus_sp,
    values_from = RII
    
  ) |>
  
  mutate(
    diff = QF - QI
  )


boot.quercus.effect <-
  
  boot.diff.quercus |>
  
  group_by(
    surv_date,
    Environment,
    Interacting_species
  ) |>
  
  group_modify(~{
    
    summarise_boot(
      .x,
      diff,
      positive_label = "Stronger effect on Q. faginea",
      negative_label = "Stronger effect on Q. ilex",
      neutral_label  = "No difference"
    )
    
  }) |>
  
  ungroup()

pd <- .5
ggplot(boot.quercus.effect, aes(x = bootstrap_mean, y = Interacting_species, colour = interpretation)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(position = position_dodge(width = pd)) +
  geom_errorbar(aes(xmin = IC2.5, xmax = IC97.5), width = .5, position = position_dodge(width = pd)) +
  #coord_cartesian(xlim = c(-1, 1)) +
  facet_grid(Environment~ surv_date) +
  labs(title = "Mean differences in RII between quercus species", 
       caption = "Positive differences indicate stronger facilitation on Q. faginea \n
       Negative differences indicate stronger competition on Q. faginea")

## 4. Differences between times

boot.diff.time <-
  
  boot.results |>
  
  pivot_wider(
    
    id_cols = c(
      iteration,
      quercus_sp,
      Environment,
      Interacting_species
    ),
    
    names_from = surv_date,
    values_from = RII
    
  ) |>
  
  transmute(
    
    iteration,
    
    quercus_sp,
    
    Environment,
    
    Interacting_species,
    
    first.winter =
      `27/05/2026` - `16/09/2025`,
    
    first.summer.and.winter =
      `27/05/2026` - `12/06/2025`,
    
    first.summer =
      `16/09/2025` - `12/06/2025`
    
  ) |>
  
  pivot_longer(
    
    -c(
      iteration,
      quercus_sp,
      Environment,
      Interacting_species
    ),
    
    names_to = "comparison",
    
    values_to = "diff"
    
  )

boot.time.effect <-
  
  boot.diff.time |>
  
  group_by(
    
    comparison,
    
    quercus_sp,
    
    Environment,
    
    Interacting_species
    
  ) |>
  
  group_modify(~{
    
    summarise_boot(
      
      .x,
      
      diff,
      
      positive_label = "Increase through time",
      
      negative_label = "Decrease through time",
      
      neutral_label = "No temporal change"
      
    )
    
  }) |>
  
  ungroup()

boot.time.effect <- boot.time.effect |> 
  mutate(comparison = factor(comparison, levels = c("first.summer", "first.winter", "first.summer.and.winter")))

pd <- .5
ggplot(boot.time.effect, aes(x = bootstrap_mean, y = Interacting_species, colour = interpretation, shape = quercus_sp)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(position = position_dodge(width = pd)) +
  geom_errorbar(aes(xmin = IC2.5, xmax = IC97.5), width = .5, position = position_dodge(width = pd)) +
  #coord_cartesian(xlim = c(-1, 1)) +
  facet_grid(Environment~comparison) +
  labs(title = "Mean differences in RII between census", 
       caption = "Positive differences indicate an increasing in facilitation through that time period \n
       Negative differences indicate an increasing in competition in that time period")

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


