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
        "<b>Precio:</b> ",precio,"<br>",
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


      graf <-  ggplot(data_filtrada, aes(x = factor(anio, levels = all_years), y = precio, fill = factor(anio, levels = all_years))) +
    geom_violin(color = "black", alpha = 0.8, width = 1, show.legend = FALSE) +
    geom_boxplot(width = 0.25, color = "black", alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(text = hover), size = 1, color = "gray40", alpha = 0.4, width = 0.15, show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        "2013" = "#0087CF",
        "2014" = "#007AB8",
        "2015" = "#006EA2",
        "2016" = "#00628C",
        "2017" = "#005776",
        "2018" = "#004C61",
        "2019" = "#00414D",
        "2020" = "#00363A",
        "2021" = "#002C28",
        "2022" = "#66B7E0",
        "2023" = "#4DAADD",
        "2024" = "#339DD9",
        "2025" = "#1A91D5"
      )
    ) +
    labs(fill = "Año", y = "Peso",x="") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  
  # --- 7. Versión interactiva con plotly ---
  graf_plotly <- plotly::ggplotly(graf, tooltip = "text") %>%
    plotly::layout(
      hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  
  

  return(graf_plotly)
}


#boxplot_interactivo(
#  data = base_precios,
#  presentacion_sel = "1 litro",
#  subgrupo_sel = "Fungicidas"
#)
