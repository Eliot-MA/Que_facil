source("01-scripts/01.2f-wood_density.R")

# Saturated model
full.model <- lm(wood.density ~ Species*Environment, data = df.wood_density)


# Model selection
library(MuMIn)
options(na.action = "na.fail")

dd <- dredge(full.model)
dd

wd.model <- get.models(dd, subset = delta < 2)[[1]]

# Report
easystats::model_dashboard(wd.model, output_dir = "07-html/", output_file = "dashborad_model_wood_density.html", browse_html = FALSE, quiet = TRUE)

