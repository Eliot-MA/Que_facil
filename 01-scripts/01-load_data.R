# What do you want to load?
## configurate this
config <- c(
  gps = "y",
  survival = "y",
  microclimate = "n"
)

scripts <- c(
  gps          = "01-scripts/01.1-gps.R",
  survival     = "01-scripts/01.2-surv_par_vol.R",
  microclimate = "01-scripts/01.3-microclimate.R"
)

for (name in names(config)) {
  if (config[name] == "y") {
    source(scripts[name])
  }
}
