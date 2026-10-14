# What do you want to load?
## configurate this
config <- c(
  abiotic = "y",
  biotic  = "y"
)

scripts <- c(
  abiotic = "01-scripts/01.1-abiotic.R",
  biotic  = "01-scripts/01.2-biotic.R"
)

for (name in names(config)) {
  if (config[name] == "y") {
    source(scripts[name])
  }
}
