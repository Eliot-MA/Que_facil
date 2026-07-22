source("01-scripts/01.2a-survival_growth.R")

mantener <- c("df.surv")
rm(list = setdiff(ls(), mantener))

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

# 3. Compute RII ----
## Create function ----
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

## Apply function to every comparison and combine ----

df.rii.qi <- pmap(comparisons, calc_rii, df = df.surv.qi) |>
  bind_rows()

df.rii.qf <- pmap(comparisons, calc_rii, df = df.surv.qf) |>
  bind_rows()

## Compute aggregated RII per individual

aggID.df.rii.qi <- df.rii.qi |> 
  group_by(ind_A, surv_date, type_RII, Interacting_species) |> 
  summarise(
    mean = mean(RII, na.rm = TRUE), 
    sd   = sd(RII, na.rm = TRUE)
  )

aggID.df.rii.qf <- df.rii.qf |> 
  group_by(ind_A, surv_date, type_RII, Interacting_species) |> 
  summarise(
    mean = mean(RII, na.rm = TRUE), 
    sd   = sd(RII, na.rm = TRUE)
  )

agg.rii.qi <- 
  aggID.df.rii.qi |> 
  filter(surv_date != "12/06/2025") |> 
  group_by(surv_date, type_RII, Interacting_species) |> 
  summarise(
    n = sum(!is.na(mean)),
    media = mean(mean, na.rm = TRUE),
    se = sd(mean, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |> 
  mutate(quercus_sp = "Q. ilex")

agg.rii.qf <- 
  aggID.df.rii.qf |> 
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


# Asegúrate de ejecutar tu código del gráfico primero:
ggplot(agg.rii, aes(x = media, y = Interacting_species, colour = quercus_sp)) + 
  geom_point(position = position_dodge(width = .5), size = 1.5) +
  geom_errorbarh(aes(xmin = media - se, xmax = media + se), width = .5, position = position_dodge(width = .5)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
  facet_grid(surv_date~type_RII) +
  ylab("") +
  xlab("RII") +
  labs(title = "Direct and indirect RII") +
  coord_cartesian(xlim = c(-.6, .6)) +
  theme_light() +
  theme(legend.position = "bottom")

# Luego, guarda el gráfico:
ggsave("08-img/eda_meanse_rii.png", width = 10, height = 8, dpi = 300)

##
# Bootstrapping inside individuals, only on controls  ----
##

library(tidyverse)

## 1. Bootstrap SOLO sobre controles ----

bootstrap_controls_only <- function(df) {
  
  df_controls   <- df |> filter(microsite == "open")
  df_treatments <- df |> filter(microsite != "open")
  
  # Resamplear solo los controles
  boot_controls <- df_controls |>
    group_by(Environment, microsite, surv_date, quercus_sp) |>
    group_modify(~ {
      boot <- slice_sample(.x, n = nrow(.x), replace = TRUE)
      boot$Individual <- paste0(boot$Individual, "_", seq_len(nrow(boot)))
      boot
    }) |>
    ungroup()
  
  # Devolver focales intactos + controles resampleados
  bind_rows(df_treatments, boot_controls)
  
}

## 2. Calcular RII para TODOS los individuos focales (sin promediar aún) ----

bootstrap_once_v3 <- function(df) {
  
  df_boot <- bootstrap_controls_only(df)
  
  # Calcular RII para cada combinación de comparisons y quercus_sp
  map_dfr(unique(df$quercus_sp), function(sp) {
    
    df_sp <- df_boot |> filter(quercus_sp == sp)
    
    pmap(comparisons, calc_rii, df = df_sp) |>
      bind_rows() |>
      mutate(quercus_sp = sp)
    
  })
  
}

## 3. Bootstrap final: 5000 iteraciones ----
## Se guarda la tabla completa con todos los individuos y todas las iteraciones
library(cli)

set.seed(123)

n_iter <- 5000

pb <- cli_progress_bar(
  name   = "Bootstrap",
  total  = n_iter,
  format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_eta_str}"
)

df <- df.surv |> 
  filter(Environment == "gap" & microsite %in% c("Genista scorpius", "open") & quercus_sp == "QI", surv_date == "16/09/2025")

boot.results <- map_dfr(
  seq_len(n_iter),
  function(i) {
    cli_progress_update(id = pb)  # <- pasar el ID explícitamente
    bootstrap_once_v3(df) |>
      mutate(iteration = i)
  }
)

cli_progress_done(id = pb)


boot.results.ind <- boot.results |> 
  group_by(ind_A, iteration) |> 
  summarise(
    boot_RII = mean(RII, na.rm = TRUE), 
    boot_lower_ci = quantile(RII, probs = .025, na.rm = TRUE),
    boot_upper_ci = quantile(RII, probs = .975, na.rm = TRUE)
  )


# Change it depending on the quercus species used
# write.csv2(boot.results, file = "00-data/boot_RII.csv")


library(dplyr)
library(ggplot2)

hist(boot.results.ind$boot_RII, breaks = 50)

p1 <- ggplot(boot.results.ind, aes(x = boot_RII)) +
  geom_histogram() +
  coord_cartesian(xlim = c(-1, 1))

p2 <- aggID.df.rii.qi |> 
  filter(type_RII == "Direct" & Interacting_species == "Genista scorpius" & surv_date == "16/09/2025") |> 
  ggplot(aes(x = mean)) +
  geom_histogram() +
  coord_cartesian(xlim = c(-1, 1)) +
  xlab("RII")

library(patchwork)
patch_plot <- p2 / p1 + plot_annotation(
  title = "Comparison of RII results. \nSubsample: Quercus ilex, Genista scorpius, gap environment, sep2025 census",
  subtitle = "A: Classic calculation \nB: Bootstrap calculation with 5000 iterations per individual",
  tag_levels = "A"
)

# Luego, guarda el gráfico:
ggsave("08-img/rii_calculation_comparison_bootstrap.png", width = 10, height = 8, dpi = 300)

df.surv |> 
  group_by(Environment, microsite, quercus_sp, surv_date) |> 
  summarise(
    n = n(), 
    n_vivo = sum(survival, na.rm = TRUE)
  ) |> 
  filter(Environment == "gap" & microsite %in% c("Genista scorpius", "open") & quercus_sp == "QI", surv_date == "12/06/2025")

df.surv |> 
  filter(Environment == "gap" & microsite == "Genista scorpius" & quercus_sp == "QI", surv_date == "12/06/2025")

vivos_gap <- df.surv |> 
  filter(Environment == "gap" & microsite == "open" & quercus_sp == "QI", surv_date == "12/06/2025") |> 
  summarise(n_vivo = sum(survival, na.rm = TRUE)) |> pull(n_vivo)

1:vivos_gap
posibles_RII <- c()

for (i in 0:vivos_gap) {
  posibles_RII[i] <- 1 - (i/30)
}

hist(posibles_RII, breaks = 150)

## 1. Calcular estadísticos acumulativos ----

convergence <- boot.results |>
  group_by(iteration, ind_A) |>
  summarise(
    RII_mean = mean(RII, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(iteration) |>
  group_by(ind_A) |>
  mutate(
    cum_mean  = cumsum(RII_mean) / seq_len(n()),
    cum_lower = sapply(seq_len(n()), function(i) quantile(RII_mean[1:i], 0.025)),
    cum_upper = sapply(seq_len(n()), function(i) quantile(RII_mean[1:i], 0.975))
  ) |>
  ungroup()

## 2. Graficar convergencia ----

ggplot(convergence, aes(x = iteration)) +
  geom_ribbon(aes(ymin = cum_lower, ymax = cum_upper), alpha = 0.2, fill = "steelblue") +
  geom_line(aes(y = cum_mean), color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(quercus_sp ~ Interacting_species + type_RII) +
  labs(
    x     = "Número de iteraciones",
    y     = "RII acumulado",
    title = "Convergencia del bootstrap"
  ) +
  theme_bw()

## 1. Calcular estadísticos acumulativos por individuo ----

convergence_ind <- boot.results |>
  filter(!is.na(RII)) |>                        # <- eliminar NAs antes
  arrange(iteration) |>
  group_by(ind_A, surv_date, type_RII, Interacting_species) |>
  mutate(
    cum_mean  = cumsum(RII) / seq_len(n()),
    cum_lower = sapply(seq_len(n()), function(i) quantile(RII[1:i], 0.025, na.rm = TRUE)),
    cum_upper = sapply(seq_len(n()), function(i) quantile(RII[1:i], 0.975, na.rm = TRUE))
  ) |>
  ungroup()

## 2. Graficar convergencia por individuo ----
convergence_ind |> 
  ggplot(aes(x = iteration)) +
  #geom_ribbon(aes(ymin = cum_lower, ymax = cum_upper), alpha = 0.05, fill = "steelblue") +
  geom_point(aes(y = cum_mean), alpha = .01) +
  geom_line(aes(y = cum_mean), color = "steelblue", alpha = 0.3, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(quercus_sp ~ Interacting_species + type_RII) +
  labs(
    x     = "Número de iteraciones",
    y     = "RII acumulado",
    title = "Convergencia del bootstrap por individuo"
  ) +
  theme_bw()

##
# Bootstrap + GLMM + RII ----
# 1. Resample ALL individuals within each (Environment x microsite x quercus_sp x surv_date)
# 2. Fit GLMM per surv_date: survival ~ quercus_sp + Environment * microsite + (1 | Individual)
# 3. Predict survival probabilities for every treatment combination
# 4. Calculate RII from predicted probabilities
##

library(glmmTMB)
library(cli)
library(purrr)

# Helper: RII from predicted probabilities (one value per group) ----
# Adapted from calc_rii: no cross-join, no averaging needed since we work
# with model-estimated group probabilities instead of individual binary data.

calc_rii_prob <- function(df_prob, env_A, ms_A, env_B, ms_B,
                          type_RII, Interacting_species) {

  focal <- df_prob |>
    filter(Environment == env_A, microsite == ms_A) |>
    select(p_A = survival, surv_date)

  ref <- df_prob |>
    filter(Environment == env_B, microsite == ms_B) |>
    select(p_B = survival, surv_date)

  inner_join(focal, ref, by = "surv_date") |>
    mutate(
      type_RII           = type_RII,
      Interacting_species = Interacting_species,
      RII = (p_A - p_B) / (p_A + p_B)
    ) |>
    select(surv_date, type_RII, Interacting_species, RII)
}

# 1. Prediction grid: all treatment combinations ----
#    Individual is included as a dummy so predict() finds the random effect term.
pred_grid <- df.surv |>
  select(Environment, microsite, quercus_sp) |>
  distinct() |>
  expand_grid(surv_date = unique(df.surv$surv_date)) |>
  mutate(Individual = "dummy")

# 2. Store results ----
all_prob  <- list()
all_rii   <- list()

# 3. Bootstrap loop ----
set.seed(123)
n_iter <- 500

pb <- cli_progress_bar(
  name   = "Bootstrap GLMM",
  total  = n_iter,
  format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_eta_str}"
)

for (i in seq_len(n_iter)) {

  cli_progress_update(id = pb)

  # 3a. Resample individuals within each group ----
  #     Appends a suffix so the same original individual can appear
  #     multiple times without breaking the (1 | Individual) structure.

  df_boot <- df.surv |>
    group_by(Environment, microsite, quercus_sp, surv_date) |>
    group_modify(~ {
      boot <- slice_sample(.x, n = nrow(.x), replace = TRUE)
      boot$Individual <- paste0(boot$Individual, "_", seq_len(nrow(boot)))
      boot
    }) |>
    ungroup()

  # 3b. Fit GLMM separately for each surv_date ----
  dates <- unique(df_boot$surv_date)
  prob_list  <- list()
  rii_list   <- list()

  for (d in dates) {

    df_d <- df_boot |> filter(surv_date == d)

    m <- tryCatch(
      glmmTMB(
        survival ~ quercus_sp + Environment * microsite +
          (1 | Individual),
        data    = df_d,
        family  = binomial()
      ),
      error   = function(e) NULL,
      warning = function(e) NULL
    )

    if (is.null(m)) next

    # 3c. Predict probabilities for the full prediction grid ----
    prob_d <- pred_grid |>
      filter(surv_date == d) |>
      mutate(
        survival = predict(m, newdata = pred_grid |> filter(surv_date == d),
                           type = "response",
                           allow.new.levels = TRUE),
        iteration = i
      )

    prob_list[[d]] <- prob_d

    # 3d. Calculate RII for each comparison ----
    rii_d <- pmap(comparisons, calc_rii_prob, df_prob = prob_d) |>
      bind_rows() |>
      mutate(iteration = i)

    rii_list[[d]] <- rii_d
  }

  all_prob[[i]] <- bind_rows(prob_list)
  all_rii[[i]]  <- bind_rows(rii_list)
}

cli_progress_done(id = pb)

# 4. Assemble results ----
boot_probs <- bind_rows(all_prob)
boot_rii   <- bind_rows(all_rii)

# 5. Summarise RII across iterations ----
boot_rii_summary <- boot_rii |>
  group_by(surv_date, type_RII, Interacting_species) |>
  summarise(
    n           = n(),
    boot_mean   = mean(RII, na.rm = TRUE),
    boot_median = median(RII, na.rm = TRUE),
    boot_sd     = sd(RII, na.rm = TRUE),
    boot_CI2.5  = quantile(RII, probs = 0.025, na.rm = TRUE),
    boot_CI97.5 = quantile(RII, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Quick look ----
print(boot_rii_summary)

ggplot(boot_rii_summary,
       aes(x = boot_mean, y = Interacting_species)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = boot_CI2.5, xmax = boot_CI97.5), height = 0.3) +
  facet_grid(surv_date ~ type_RII) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(x = "RII (bootstrap mean ± 95% CI)", y = "") +
  theme_light()

##
# Convergence analysis (GLMM bootstrap) ----
##

# 1. Cumulative statistics per comparison group ----

convergence_glmm <- boot_rii |>
  filter(!is.na(RII)) |>
  arrange(iteration) |>
  group_by(surv_date, type_RII, Interacting_species) |>
  mutate(
    iter_in_group = seq_len(n()),
    cum_mean  = cumsum(RII) / iter_in_group,
    cum_lower = sapply(iter_in_group, function(j) quantile(RII[1:j], 0.025)),
    cum_upper = sapply(iter_in_group, function(j) quantile(RII[1:j], 0.975))
  ) |>
  ungroup()

# 2. Plot convergence ----

ggplot(convergence_glmm, aes(x = iteration)) +
  geom_ribbon(aes(ymin = cum_lower, ymax = cum_upper), alpha = 0.2, fill = "steelblue") +
  geom_line(aes(y = cum_mean), color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(Interacting_species ~ surv_date + type_RII) +
  labs(
    x     = "Iterations",
    y     = "Cumulative RII",
    title = "Bootstrap convergence (GLMM-based RII)"
  ) +
  theme_bw()

# 3. Save both plots to PDF ----

pdf("08-img/bootstrap_glmm_RII.pdf", width = 12, height = 10)

# --- Plot 1: RII estimates ---
p1 <- ggplot(boot_rii_summary,
             aes(x = boot_mean, y = Interacting_species)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = boot_CI2.5, xmax = boot_CI97.5), height = 0.3) +
  facet_grid(surv_date ~ type_RII) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(x = "RII (bootstrap mean ± 95% CI)", y = "",
       title = "Relative Interaction Index (RII) estimated via GLMM bootstrap") +
  theme_light()
print(p1)

# --- Plot 2: Convergence ---
p2 <- ggplot(convergence_glmm, aes(x = iteration)) +
  geom_ribbon(aes(ymin = cum_lower, ymax = cum_upper), alpha = 0.2, fill = "steelblue") +
  geom_line(aes(y = cum_mean), color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(Interacting_species ~ surv_date + type_RII) +
  labs(x = "Iterations", y = "Cumulative RII",
       title = "Bootstrap convergence (GLMM-based RII)") +
  theme_bw()
print(p2)

# --- Explanatory text page ---
par(mar = c(5, 5, 5, 5))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(0.5, 0.95,
     "GLMM Bootstrap RII — Methodology and Interpretation",
     cex = 1.6, font = 2, adj = 0.5)
text(0.5, 0.82,
     paste("These results were obtained by resampling all individuals (shrubs and \n",
           "controls) with replacement within each combination of environment,\n",
           "microsite, Quercus species and census date. For each of the 500 \n",
           "bootstrap iterations, a GLMM was fitted separately per census date:\n"),
     cex = 1.05, adj = 0.5)
text(0.5, 0.68,
     "survival ~ quercus_sp + Environment * microsite + (1 | Individual) \n",
     cex = 1.1, font = 3, adj = 0.5)
text(0.5, 0.58,
     paste("From each fitted model, survival probabilities were predicted for every \n",
           "treatment combination. The RII was then computed from these predicted \n",
           "probabilities for seven biologically meaningful comparisons: \n"),
     cex = 1.05, adj = 0.5)
text(0.5, 0.42,
     paste("Direct effects of Cistus ladanifer, Rosa canina and Genista scorpius \n",
           "(shrub vs. open in gap). Direct effect of Pinus pinaster \n",
           "(canopy vs. gap in open). Indirect effects of the three shrub species \n",
           "(shrub vs. open under pine canopy)."),
     cex = 1.0, adj = 0.5)
text(0.5, 0.25,
     paste("Plot 1: Bootstrap mean RII and 95% CI for each comparison. \n",
           "Values above the dashed red line indicate facilitation; \n",
           "values below indicate competition.",
           "\n\nPlot 2: Cumulative mean and confidence bounds across iterations, \n",
           "to assess whether 500 iterations are sufficient for stable estimates. \n"),
     cex = 1.05, adj = 0.5)
text(0.5, 0.06,
     paste("Positive RII = facilitation  |  Negative RII = competition  |  RII = 0 = neutral \n",
           "\nBootstrap iterations: 500  |  Seed: 123"),
     cex = 0.9, col = "grey40", adj = 0.5)

dev.off()

cat("PDF saved to 08-img/bootstrap_glmm_RII.pdf\n")

