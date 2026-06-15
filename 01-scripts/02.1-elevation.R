source("01-scripts/01.1-elevation.R")

library(tidyverse)
library(terra)
library(sf)
library(tidylog)
library(plotly)

# Exploratory analysis
summ.elevation <- df.shrubs.gps |> 
  group_by(Zone, microsite_code) |> 
  summarise(
    minimo = min(Elevation), 
    q25 = quantile(Elevation, 0.25), 
    q50 = quantile(Elevation, 0.50), 
    media = mean(Elevation), 
    q75 = quantile(Elevation, 0.75), 
    maximo = max(Elevation)
  )

write.csv2(summ.elevation, file = "00-data/summ.elevation.csv")

summ.slope <- df.shrubs.gps |> 
  group_by(Zone, microsite_code) |> 
  summarise(
    minimo = min(Slope), 
    q25 = quantile(Slope, 0.25), 
    q50 = quantile(Slope, 0.50), 
    media = mean(Slope), 
    q75 = quantile(Slope, 0.75), 
    maximo = max(Slope)
  )

# Boxplot, elevation
p <- ggplot(df.shrubs.gps, aes(x = Elevation, y = microsite_code)) +
  geom_boxplot() +
  geom_violin(alpha = 0.2) +
  geom_jitter() +
  facet_wrap(~Zone, ncol = 1)

ggsave(
  filename = "08-img/boxplot_elevation.png",
  plot = p,
  width = 8,
  height = 10,
  dpi = 300
)

# Boxplot, slope
p <- ggplot(df.shrubs.gps, aes(x = Slope, y = microsite_code)) +
  geom_boxplot() +
  geom_violin(alpha = 0.2) +
  geom_jitter() +
  facet_wrap(~Zone, ncol = 1)

ggsave(
  filename = "08-img/boxplot_slope.png",
  plot = p,
  width = 8,
  height = 10,
  dpi = 300
)

# 3d graph
library(plotly)
range.y <- max(df.shrubs.gps$Y) - min(df.shrubs.gps$Y)
range.el <- max(df.shrubs.gps$Elevation) - min(df.shrubs.gps$Elevation)
f <- range.el / range.y

p3d <- plot_ly(
  data = df.shrubs.gps,
  x = ~X,
  y = ~Y,
  z = ~Elevation,
  type = "scatter3d",
  mode = "markers",
  color = ~microsite_code
) |> 
  plotly::layout(
    scene = list(
      aspectratio = list(
        x = 1,
        y = 1,
        z = f * 2
      )
    )
  )


# convertir puntos una sola vez
pts_sf <- sf::st_as_sf(
  df.shrubs.gps,
  coords = c("X", "Y"),
  crs = 25830
)

# valores de n a explorar
n.values <- c(2, 3)

for(n in n.values){
  
  # cuantiles
  q.elevation <- quantile(
    df.shrubs.gps$Elevation,
    probs = seq(0, 1, by = 1/n)
  )[-c(1, n+1)]
  
  # curvas de nivel
  cont <- terra::as.contour(
    dem_crop,
    levels = q.elevation
  )
  
  cont_sf <- sf::st_as_sf(cont)
  
  # gráfico
  p <- ggplot() +
    
    geom_sf(
      data = cont_sf,
      aes(color = level),
      linewidth = 1
    ) +
    
    geom_sf(
      data = pts_sf,
      aes(color = Elevation),
      size = 2
    ) +
    
    scale_color_viridis_c() +
    
    theme_minimal() +
    
    labs(
      title = "Elevation contour lines",
      subtitle = paste0(
        n - 1,
        " elevation classes defined by quantiles: ",
        paste(names(q.elevation), collapse = ", "),
        "\nat ",
        paste(round(q.elevation, 1), collapse = ", "),
        " m respectively"
      )
    )
  
  # nombre archivo
  file.name <- paste0(
    "08-img/elevation_quantiles_n",
    n,
    ".png"
  )
  
  # guardar
  ggsave(
    filename = file.name,
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
}
