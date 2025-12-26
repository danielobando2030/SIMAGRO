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
               glue,  tidyverse, gridExtra, corrplot, plotly,
               shiny)
options(scipen = 999)

###############################
##### Setting directories #####
###############################

###############################
##### Load and process data ###
###############################

data <- readRDS("base_diaria_mayoristas_indices_bog_3_6.rds")

data <- data[order(data$producto, data$fecha), ]

visualizar_bandas_plotly <- function(data, producto_sel, anio_sel = NULL) {
  
  meses_es <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
  
  df <- data %>%
    filter(producto == producto_sel) %>%
    mutate(anio = year(fecha)) %>%
    arrange(fecha) %>%
    group_by(anio) %>%
    mutate(
      media_20 = rollapply(precio, width = 20, FUN = mean,
                           align = "right", fill = NA, na.rm = TRUE),
      sd_20    = rollapply(precio, width = 20, FUN = sd,
                           align = "right", fill = NA, na.rm = TRUE),
      precio_norm = precio - media_20,
      banda_sup =  2 * sd_20,
      banda_inf = -2 * sd_20,
      estado = case_when(
        is.na(precio_norm) ~ NA_character_,  
        precio_norm > banda_sup | precio_norm < banda_inf ~ "Atípico",
        TRUE ~ "Normal"
      ),
      label_fecha = paste0(day(fecha), "-", meses_es[month(fecha)])
    ) %>%
    ungroup()
  
  if (!is.null(anio_sel)) {
    df <- df %>% filter(anio == anio_sel)
  }
  
  df_plot <- df %>% filter(!is.na(estado))
  
  fechas_ticks <- df_plot$fecha[seq(1, nrow(df_plot), by = 10)]
  labels_ticks <- df_plot$label_fecha[seq(1, nrow(df_plot), by = 10)]
  
  fig <- plot_ly(df_plot, x = ~fecha) %>%
    
    add_ribbons(
      ymin = ~banda_inf, ymax = ~banda_sup,
      fillcolor = "rgba(255,221,0,0.25)",
      line = list(color = "rgba(255,221,0,0)"),
      name = "Banda ±2sd",
      hoverinfo = "none",
      showlegend = TRUE
    ) %>%
    
    add_lines(
      y = ~precio_norm,
      name = "Precio normalizado",
      line = list(color = "#33322C", width = 2),
      showlegend = TRUE
    ) %>%
    
    add_markers(
      y = ~precio_norm,
      name = "Precio normalizado",
      marker = list(
        color = ~ifelse(estado == "Atípico", "#DBC21F", "#33322C"),
        size = 6
      ),
      text = ~paste(
        "<b>Fecha:</b> ", label_fecha,
        "<br><b>Precio normalizado:</b> ", 
        format(precio_norm, big.mark=".", decimal.mark=",", nsmall=2),
        "<br><b>Estado:</b> ", estado
      ),
      hoverinfo = "text",
      showlegend = FALSE
    ) %>%
    
    layout(
      title = NULL,
      xaxis = list(
        title = "Fecha",
        tickangle = -45,
        tickvals = fechas_ticks,
        ticktext = labels_ticks
      ),
      yaxis = list(
        title = "Precio normalizado",
        tickformat = ".0f",
        separators = ",."
      ),
      hovermode = "closest",
      legend = list(title = list(text = ""))
    )
  
  return(fig)
}


visualizar_bandas_estatico <- function(data, producto_sel, anio_sel = NULL) {
  
  meses_es <- c("Ene","Feb","Mar","Abr","May","Jun",
                "Jul","Ago","Sep","Oct","Nov","Dic")
  
  df <- data %>%
    filter(producto == producto_sel) %>%
    mutate(anio = year(fecha)) %>%
    arrange(fecha) %>%
    group_by(anio) %>%
    mutate(
      media_20 = zoo::rollapply(precio, 20, mean,
                                align = "right", fill = NA, na.rm = TRUE),
      sd_20    = zoo::rollapply(precio, 20, sd,
                                align = "right", fill = NA, na.rm = TRUE),
      precio_norm = precio - media_20,
      banda_sup =  2 * sd_20,
      banda_inf = -2 * sd_20,
      estado = case_when(
        is.na(precio_norm) ~ NA_character_,
        precio_norm > banda_sup | precio_norm < banda_inf ~ "Atípico",
        TRUE ~ "Normal"
      ),
      label_fecha = paste0(day(fecha), "-", meses_es[month(fecha)])
    ) %>%
    ungroup()
  
  if (!is.null(anio_sel)) {
    df <- df %>% filter(anio == anio_sel)
  }
  
  df_plot <- df %>% filter(!is.na(estado))
  
  if (nrow(df_plot) == 0) {
    return(ggplot() + theme_void())
  }
  
  ggplot(df_plot, aes(x = fecha)) +
    
    # Banda ±2 sd
    geom_ribbon(
      aes(ymin = banda_inf, ymax = banda_sup),
      fill = "#FFDD00",
      alpha = 0.25
    ) +
    
    # Línea precio normalizado
    geom_line(
      aes(y = precio_norm),
      color = "#33322C",
      linewidth = 0.8
    ) +
    
    # Puntos
    geom_point(
      aes(y = precio_norm, color = estado),
      size = 2
    ) +
    
    scale_color_manual(
      values = c(
        "Normal"  = "#33322C",
        "Atípico" = "#DBC21F"
      )
    ) +
    
    scale_y_continuous(
      name = "Precio normalizado",
      labels = function(x)
        formatC(x, big.mark = ".", decimal.mark = ",", digits = 0, format = "f")
    ) +
    
    scale_x_date(
      name = "Fecha",
      date_breaks = "10 days",
      date_labels = "%d-%b"
    ) +
    
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}

# Ejemplo
visualizar_bandas_plotly(data, "Aguacate", 2014)
