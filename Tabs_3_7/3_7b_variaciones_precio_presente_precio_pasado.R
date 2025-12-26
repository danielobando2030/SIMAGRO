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

formato_es <- function(x, dec = 2) {
  format(
    round(x, dec),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE
  )
}

calc_cambio_mensual <- function(data, producto, anio) {
  
  Sys.setlocale("LC_TIME", "Spanish_Colombia.UTF-8")
  if (Sys.getlocale("LC_TIME") == "C") Sys.setlocale("LC_TIME", "Spanish")
  
  df <- data %>%
    mutate(
      mes_y_ano = as.Date(paste0(mes_y_ano, "-01")),
      anio = year(mes_y_ano)
    ) %>%
    filter(
      tolower(producto) == tolower(!!producto),
      anio == !!anio
    ) %>%
    arrange(mes_y_ano) %>%
    mutate(
      cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100
    ) %>%
    filter(!is.na(cambio_pct_mensual))
  
  if (nrow(df) == 0) return(NULL)
  
  # Tooltip con formato español
  df <- df %>%
    mutate(
      text_label = paste0(
        "Mes: ", tools::toTitleCase(format(mes_y_ano, "%B %Y")),
        "<br>Precio promedio: $", formato_es(precio_prom, 0),
        "<br>Cambio mensual: ", formato_es(cambio_pct_mensual, 2), "%"
      )
    )
  
  # Valores bonitos para el eje Y
  ticks_y <- pretty(df$cambio_pct_mensual)
  
  # Gráfico
  fig <- plot_ly(
    data = df,
    x = ~mes_y_ano,
    y = ~cambio_pct_mensual,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "#DBC21F", width = 2),
    marker = list(color = "#DBC21F", size = 8),
    text = ~text_label,
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    
    layout(
      title = NULL,
      xaxis = list(
        title = "Mes",
        tickvals = df$mes_y_ano,
        ticktext = tools::toTitleCase(format(df$mes_y_ano, "%b")),
        tickangle = -45
      ),
      yaxis = list(
        title = "Cambio % mensual",
        tickvals = ticks_y,
        ticktext = formato_es(ticks_y, 2),
        
        # ⭐⭐ NUEVO: AMPLIAR EJE Y PARA QUE SE VEAN TODOS LOS LABELS ⭐⭐
        range = c(min(ticks_y) - 5, max(ticks_y) + 5),
        dtick = 5,
        
        zeroline = FALSE
      ),
      hovermode = "x unified"
    )
  
  fig$x$layout$title <- NULL
  return(fig)
}

###############################
##### ESTÁTICO              ###
###############################

calc_cambio_mensual_estatico <- function(data, producto, anio) {
  
  Sys.setlocale("LC_TIME", "Spanish_Colombia.UTF-8")
  if (Sys.getlocale("LC_TIME") == "C") Sys.setlocale("LC_TIME", "Spanish")
  
  df <- data %>%
    mutate(
      mes_y_ano = as.Date(paste0(mes_y_ano, "-01")),
      anio = year(mes_y_ano)
    ) %>%
    filter(
      tolower(producto) == tolower(!!producto),
      anio == !!anio
    ) %>%
    arrange(mes_y_ano) %>%
    mutate(
      cambio_pct_mensual = (precio_prom - lag(precio_prom)) / lag(precio_prom) * 100
    ) %>%
    filter(!is.na(cambio_pct_mensual))
  
  if (nrow(df) == 0) {
    return(ggplot() + theme_void())
  }
  
  # breaks “bonitos” eje Y
  breaks_y <- pretty(df$cambio_pct_mensual)
  
  ggplot(df, aes(x = mes_y_ano, y = cambio_pct_mensual)) +
    
    geom_line(
      color = "#DBC21F",
      linewidth = 1
    ) +
    
    geom_point(
      color = "#DBC21F",
      size = 3
    ) +
    
    scale_x_date(
      name = "Mes",
      date_breaks = "1 month",
      date_labels = "%b",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    scale_y_continuous(
      name = "Cambio % mensual",
      breaks = breaks_y,
      labels = function(x)
        formatC(x, big.mark = ".", decimal.mark = ",", digits = 2, format = "f"),
      limits = c(min(breaks_y) - 5, max(breaks_y) + 5)
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_blank()
    )
}




