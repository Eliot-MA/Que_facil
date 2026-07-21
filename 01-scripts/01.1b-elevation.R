# Libraries
library(tidyverse)

cat("Loading data from digital terrain model.\n")

# Load digital terrain model 5m
r <- terra::rast("00-data/MDT02-ETRS89-HU30-0486-1-COB2.tif")

# Load shrub positions
df.shrubs.gps <- read.csv2(file = "00-data/shrubs_gps.csv")

## Change variable names and
## codification of zone
df.shrubs.gps <- df.shrubs.gps |> 
  select(-X.1, -RP) |> 
  rename(
    Individual     = Tratamiento, 
    Zone           = Ambiente, 
    microsite_code = SP
  ) |> 
  mutate(
    Zone = recode(
      Zone, 
      "abierto" = "open", 
      "pinar "   = "pine canopy"
    )
  )

pts <- sf::st_as_sf(
  df.shrubs.gps,
  coords = c("X", "Y"),
  crs = 25830
)

# Convert points to SpatVector
vpts <- terra::vect(pts)

dem_crop <- terra::crop(
  r,
  terra::ext(vpts) + 25  
)


dem_mask <- terra::mask(dem_crop, vpts)

# Compute slope
slope <- terra::terrain(
  dem_crop,
  v = "slope",
  unit = "degrees"
)

# Save elevation map
png("08-img/map_shrubs_elevation.png", 
    width = 960, height = 960)

terra::plot(dem_crop, main = "MDT02 cropped")
terra::plot(vpts, add = TRUE, col = "red", pch = 16)

dev.off()

# Save slope map
png("08-img/map_shrubs_slope.png", 
    width = 960, height = 960)

terra::plot(slope, main = "MDT02, computed slope (degrees), cropped")
terra::plot(vpts, add = TRUE, col = "red", pch = 16)

dev.off()

# extract elevation and slope data
elev <- terra::extract(dem_crop, vpts)
slop <- terra::extract(   slope, vpts)
df.shrubs.gps$Elevation <- elev[,2]
df.shrubs.gps$Slope     <- slop[,2]

# Eliminate not necessary objects
# rm(list = setdiff(ls(), "df.shrubs.gps"))

cat("Done. 'df.shrubs.gps' now includes Elevation and Slope columns.\n")
