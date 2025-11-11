######
# Author:       Luis Miguel García
#               Laura Quintero
#               Daniel Obando
# First Edited: 2025/08/24
# Last Editor:  2025/09/12
# R version:    4.3.2
######

###############################
##### Libraries ###############
###############################

rm(list=ls())

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl,
               glue,  tidyverse, gridExtra, corrplot, plotly,
               treemapify, arrow)
options(scipen = 999)

###############################
##### Setting directories #####
###############################

##############################

base_precios <- readRDS("data_precios_insumos_3_10.rds") 
head(base_precios)

# ---- Función ----
boxplot_interactivo <- function(data, presentacion_sel, subgrupo_sel) {
  
  # Filtrar por selección
  data_filtrada <- data %>%
    filter(
      presentacion == presentacion_sel,
      subgrupos == subgrupo_sel
    )
  
  if (nrow(data_filtrada) == 0) {
    stop("No hay datos para esa combinación de presentación y subgrupo.")
  }
  
  all_years <- sort(unique(data$anio))
  
  # Calcular estadísticas por año
  stats <- data_filtrada %>%
    group_by(anio) %>%
    summarise(
      n = n(),
      mediana = median(precio, na.rm = TRUE),
      q1 = quantile(precio, 0.25, na.rm = TRUE),
      q3 = quantile(precio, 0.75, na.rm = TRUE),
      minimo = min(precio, na.rm = TRUE),
      maximo = max(precio, na.rm = TRUE),
      municipio_min = municipio[which.min(precio)],
      municipio_max = municipio[which.max(precio)]
    )
  
  # Unir estadísticas a cada observación
  data_filtrada <- data_filtrada %>%
    left_join(stats, by = "anio") %>%
    mutate(
      hover = paste0(
        "<b>Año:</b> ", anio, "<br>",
        "<b>Mediana:</b> ", format(round(mediana, 0), big.mark = ".", decimal.mark = ","), "<br>",
        "<b>Q1:</b> ", format(round(q1, 0), big.mark = ".", decimal.mark = ","), "<br>",
        "<b>Q3:</b> ", format(round(q3, 0), big.mark = ".", decimal.mark = ","), "<br>",
        "<b>Mínimo:</b> ", format(round(minimo, 0), big.mark = ".", decimal.mark = ","), 
        " (", municipio_min, ")<br>",
        "<b>Máximo:</b> ", format(round(maximo, 0), big.mark = ".", decimal.mark = ","), 
        " (", municipio_max, ")<br>",
        "<b>Número de observaciones:</b> ", n
      )
    )
  
  # Crear boxplot interactivo
  fig <- plot_ly(
    data = data_filtrada,
    x = ~factor(anio, levels = all_years),
    y = ~precio,
    type = "box",
    boxpoints = "all",
    jitter = 0.3,
    pointpos = -1.8,
    fillcolor = 'rgba(186, 85, 211, 0.5)',   # púrpura claro
    line = list(color = 'rgba(75, 0, 130, 1)', width = 2),  # bordes púrpura oscuro
    marker = list(color = 'rgba(75, 0, 130, 0.5)', size = 4),
    median = list(color = 'rgba(75, 0, 130, 1)', width = 3),
    text = ~hover,
    hoverinfo = "text"
  ) %>%
    layout(
      title = paste0("Distribución de precios por año<br>",
                     subgrupo_sel, " | ", presentacion_sel),
      xaxis = list(title = "Año", categoryorder = "array", categoryarray = all_years),
      yaxis = list(title = "Precio"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  
  return(fig)
}


boxplot_interactivo(
  data = base_precios,
  presentacion_sel = "1 litro",
  subgrupo_sel = "Fungicidas"
)
