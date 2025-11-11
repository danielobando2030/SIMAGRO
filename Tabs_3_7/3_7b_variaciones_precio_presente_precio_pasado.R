######
# Author:       Luis Miguel García
#               Laura Quintero
#               Daniel Obando
# First Edited: 2025/08/24
# Last Editor:  2025/09/12
# R version:    4.3.2
######

if (interactive()) {
  rm(list=ls())
}

###############################
##### Libraries ###############
###############################

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl,
               glue,  tidyverse, gridExtra, corrplot, plotly)
options(scipen = 999)

###############################
##### Setting directories #####
###############################


###############################
##### Load and process data ###
###############################

data <- readRDS("base_precios_mayorista_mes_filtrados_3_7.rds")

head(data)

## Función

calc_cambio_mensual <- function(data, producto, anio) {
  
  # Establecer idioma de los meses en español
  Sys.setlocale("LC_TIME", "Spanish_Colombia.UTF-8")
  if (Sys.getlocale("LC_TIME") == "C") {
    Sys.setlocale("LC_TIME", "Spanish")
  }
  
  df <- data %>%
    mutate(
      mes_y_ano = as.Date(paste0(mes_y_ano, "-01")),
      anio = year(mes_y_ano)
    ) %>%
    filter(tolower(producto) == tolower(!!producto), anio == !!anio) %>%
    arrange(mes_y_ano) %>%
    mutate(
      cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100
    ) %>%
    filter(!is.na(cambio_pct_mensual))
  
  if (nrow(df) == 0) {
    warning(paste("No hay datos para", producto, "en", anio))
    return(NULL)
  }
  
  df <- df %>%
    mutate(
      text_label = paste0(
        "Mes: ", tools::toTitleCase(format(mes_y_ano, "%B %Y")),
        "<br>Precio promedio: ", round(precio_prom, 2),
        "<br>Cambio % mensual: ", round(cambio_pct_mensual, 2), "%"
      )
    )
  
  fig <- plot_ly(df,
                 x = ~mes_y_ano,
                 y = ~cambio_pct_mensual,
                 type = 'scatter',
                 mode = 'lines+markers',
                 line = list(color = '#9B30FF', width = 2),
                 marker = list(color = '#9B30FF', size = 8),
                 text = ~text_label,
                 hoverinfo = 'text',
                 showlegend = FALSE) %>%
    
    add_lines(y = 0,
              x = ~mes_y_ano,
              line = list(color = 'rgba(155,48,255,0.3)', dash = 'dash'),
              showlegend = FALSE,
              inherit = FALSE,
              hoverinfo = "none") %>%
    
    layout(
      title = list(text = ""),  # 👈 sin título
      xaxis = list(
        title = "Mes",
        tickvals = df$mes_y_ano,
        ticktext = tools::toTitleCase(format(df$mes_y_ano, "%b")),
        tickangle = -45,
        tickfont = list(size = 12)
      ),
      yaxis = list(title = "Cambio % mensual"),
      hovermode = "x unified"
    )
  
  return(fig)
}

#calc_cambio_mensual(data, producto = "Aguacate", anio = 2014)






