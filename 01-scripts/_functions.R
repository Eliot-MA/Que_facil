library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)
library(rlang)

plot_distribution_stats <- function(data, variable, bins = 20) {
  
  # Capturar variable
  var <- enquo(variable)
  var_name <- as_name(var)
  
  # Resumen estadístico
  summ <- data |> 
    summarise(
      minimo    = min(!!var, na.rm = TRUE),
      q25       = quantile(!!var, probs = 0.25, na.rm = TRUE), 
      mediana   = median(!!var, na.rm = TRUE),
      media     = mean(!!var, na.rm = TRUE),
      q75       = quantile(!!var, probs = 0.75, na.rm = TRUE),
      maximo    = max(!!var, na.rm = TRUE), 
      sd        = sd(!!var, na.rm = TRUE), 
      asimetria = skewness(!!var, na.rm = TRUE), 
      kurtosis  = kurtosis(!!var, na.rm = TRUE)
    )
  
  # Datos para líneas
  lines_df <- summ |> 
    select(minimo, q25, mediana, media, q75, maximo) |> 
    pivot_longer(
      cols = everything(),
      names_to = "estadistico",
      values_to = "valor"
    )
  
  # Texto resumen
  label_stats <- paste0(
    "Mean = ", round(summ$media, 2),
    "\nSD = ", round(summ$sd, 2),
    "\nSkewness = ", round(summ$asimetria, 2),
    "\nKurtosis = ", round(summ$kurtosis, 2)
  )
  
  hist_info <- hist(pull(data, !!var), plot = FALSE, breaks = bins)
  y_min <- min(hist_info$counts)
  
  # Gráfico
  ggplot(data, aes(x = !!var)) +
    
    geom_histogram(
      bins = bins,
      alpha = 0.8
    ) +
    
    geom_vline(
      data = lines_df,
      aes(
        xintercept = valor,
        color = estadistico
      ),
      linewidth = 1,
      linetype = "dashed",
      show.legend = FALSE
    ) +
    
    geom_text(
      data = lines_df,
      aes(
        x = valor,
        y = 1,
        label = estadistico,
        color = estadistico
      ),
      angle = 90,
      vjust = 1.5,
      size = 3,
      show.legend = FALSE
    ) +
    
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = label_stats,
      hjust = 1.1,
      vjust = 1.2,
      size = 4
    ) +
    
    labs(
      title = paste("Distribution of", var_name),
      x = var_name,
      y = "Count"
    ) +
    
    theme_minimal()
}

