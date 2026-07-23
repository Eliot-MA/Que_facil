source("01-scripts/01.2c-SLA.R")

full.lm.sla <- lm(SLA ~ shrub * Environment, data = df.sla)

library(MuMIn)

options(na.action = "na.fail")

dd <- dredge(full.lm.sla)

lm.sla <- get.models(dd, subset = delta < 2)[[1]]

easystats::model_dashboard(lm.sla, output_dir = "07-html/", output_file = "dashborad_model_SLA.html", browse_html = FALSE, quiet = TRUE)