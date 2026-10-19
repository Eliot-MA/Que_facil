# Supplementary analysis: RII on growth data
# This script depends on objects created in:
source("01-scripts/01.2a-survival_growth.R")

# RII on growth is expected to be more appropriate than on survival, because
# growth is a continuous (positive) measure, matching the index's assumptions.
#
# TODO (not developed yet):
#   1. Choose the growth response (e.g. height/volume increment) from df.surv
#      / df.vol / df.biomass.
#   2. Reuse the `comparisons` table and the RII formula
#      RII = (X_A - X_B) / (X_A + X_B) with X = growth under focal vs reference.
#   3. Compare against the growth models in 03-scripts.
#
# See 05-supplementary/RII_survival.R for the structure to follow.
