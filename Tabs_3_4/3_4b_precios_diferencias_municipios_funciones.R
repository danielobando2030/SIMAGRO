######
# Author:       Luis Miguel García
#               Laura Quintero
#               Daniel Obando
# First Edited: 2025/08/24
# Last Editor:  2025/11/xx
# R version:    4.3.2
######

###############################
##### Libraries ###############
###############################

rm(list = ls())

pacman::p_load(
  readr, readxl, reshape2, ggplot2, gganimate, dplyr, tidyr, janitor,
  lubridate, zoo, stringr, ggrepel, plotly, scales
)

###############################
##### Cargar bases ###########
###############################

data_comparacion_anual_mensual_producto <- readRDS("base_precios_data_comparacion_mensual_producto_3_4.rds")
data_comparacion_anual_mensual          <- readRDS("base_precios_data_comparacion_anual_mensual_3_4.rds")
data_comparacion_mensual_producto       <- readRDS("data_comparacion_mensual_producto_3_4.rds")
data_comparacion_mensual                <- readRDS("data_comparacion_mensual_3_4.rds")
data_comparacion_anual_producto         <- readRDS("data_comparacion_anual_producto_3_4.rds")
data_comparacion_anual                  <- readRDS("data_comparacion_anual_3_4.rds")
data_comparacion_producto               <- readRDS("data_comparacion_producto_3_4.rds")
data_comparacion                        <- readRDS("data_comparacion_3_4.rds")

################################################################################-
##### FUNCIÓN INTERACTIVA #######################################################
################################################################################-

diferencias_precios_interactivo <- function(opcion1, opcion2 = NULL, opcion4 = NULL) {
  
  city_colors <- c(
    "#DBC21F", "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
    "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22",
    "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896",
    "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7"
  )
  
  # ---- Selección de datos ----
  if (opcion1 == 1 & (is.null(opcion4) | opcion4 == "Todos") & is.null(opcion2)) {
    df <- data_comparacion
  } else if (opcion1 == 1 & !is.null(opcion2)) {
    df <- data_comparacion_anual %>% filter(year == opcion2)
  } else if (opcion1 == 0 & (is.null(opcion4) | opcion4 == "Todos") & is.null(opcion2)) {
    df <- data_comparacion_producto %>%
      group_by(ciudad) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  } else if (opcion1 == 0 & !is.null(opcion4) & is.null(opcion2)) {
    df <- data_comparacion_producto %>% filter(producto == opcion4)
  } else if (opcion1 == 0 & !is.null(opcion2) & !is.null(opcion4)) {
    df <- data_comparacion_anual_producto %>% filter(producto == opcion4 & year == opcion2)
  } else {
    df <- tibble()
  }
  
  # ---- Sin datos ----
  if (nrow(df) == 0) {
    p <- plot_ly() %>%
      layout(
        annotations = list(
          text = "❌ Sin datos disponibles para los criterios seleccionados",
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 18, color = "#DBC21F")
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
    return(list(grafico = p, datos = NULL))
  }
  
  # ---- Renombrar columnas ----
  sd_col   <- names(df)[stringr::str_detect(names(df), "^sd_|^sd_bogota|^dev")]
  comp_col <- names(df)[stringr::str_detect(names(df), "^comparacion|^comp")]
  if (length(sd_col) > 0)   names(df)[names(df) == sd_col[1]]   <- "dev"
  if (length(comp_col) > 0) names(df)[names(df) == comp_col[1]] <- "comp"
  
  # ---- SD promedio y Bogotá ----
  mean_dev_others <- df %>%
    filter(ciudad != "Bogotá") %>%
    summarise(mean_dev = mean(dev, na.rm = TRUE)) %>%
    pull(mean_dev)
  
  if (!("Bogotá" %in% df$ciudad)) {
    df <- bind_rows(df, tibble(ciudad = "Bogotá", comp = 0, dev = mean_dev_others))
  } else {
    df$comp[df$ciudad == "Bogotá"] <- 0
    if (any(is.na(df$dev[df$ciudad == "Bogotá"])) || all(df$dev[df$ciudad == "Bogotá"] == 0)) {
      df$dev[df$ciudad == "Bogotá"] <- mean_dev_others
    }
  }
  
  # ---- Tamaño burbujas ----
  if (all(is.na(df$dev)) || sd(df$dev, na.rm = TRUE) == 0) {
    df$marker_size <- rep(60, nrow(df))
  } else {
    df$marker_size <- scales::rescale(df$dev, to = c(30, 100))
  }
  
  mean_marker_others <- mean(df$marker_size[df$ciudad != "Bogotá"], na.rm = TRUE)
  df$marker_size[df$ciudad == "Bogotá"] <- mean_marker_others
  
  # ---- Tooltip ----
  df$tooltip_text <- ifelse(
    df$ciudad == "Bogotá",
    paste0(
      "Ciudad: <b>Bogotá</b>",
      "<br>Diferencia con Bogotá: $0",
      "<br>Desviación estándar (promedio nacional): ",
      formatC(mean_dev_others, big.mark = ",", decimal.mark = ".", format = "f", digits = 0)
    ),
    paste0(
      "Ciudad: <b>", df$ciudad, "</b>",
      "<br>Diferencia con Bogotá: $",
      formatC(df$comp, big.mark = ",", decimal.mark = ".", format = "f", digits = 0),
      "<br>Desviación estándar: ",
      formatC(df$dev, big.mark = ",", decimal.mark = ".", format = "f", digits = 0)
    )
  )
  
  # ---- Eliminar duplicados explícitamente ----
  df <- df %>% distinct(ciudad, .keep_all = TRUE)
  
  # ---- Colores ----
  df$color <- city_colors[seq_len(nrow(df))]
  df$color[df$ciudad == "Bogotá"] <- "#DBC21F"
  
  # ---- Gráfico ----
  p <- plot_ly(
    data = df,
    x = ~comp,
    y = 1,
    type = "scatter",
    mode = "markers",
    marker = list(
      size  = ~marker_size,
      color = ~color,
      line  = list(color = ~color, width = 2)
    ),
    text = ~tooltip_text,
    hoverinfo = "text"
  )
  
  # ---- Espaciado (AUMENTADO) ----
  # ---- Espaciado dinámico ----
  offset_y_base <- 0.1   # antes 0.08, ahora MUCHO más separado
  
  bogota_mostrada <- FALSE
  
  for (i in seq_len(nrow(df))) {
    
    ciudad_i <- df$ciudad[i]
    
    # offset adicional según tamaño del círculo (más grande = más separación)
    offset_extra <- df$marker_size[i] / 400   # escala suave
    offset_total <- offset_y_base + offset_extra
    
    # --- Etiqueta de Bogotá SOLO UNA VEZ ---
    if (ciudad_i == "Bogotá") {
      if (!bogota_mostrada) {
        p <- p %>% add_annotations(
          x = df$comp[i],
          y = 1 + offset_total + 0.03,
          text = ciudad_i,
          showarrow = FALSE,
          textangle = 90,
          font = list(size = 13, family = "Prompt", color = "#333333")
        )
        bogota_mostrada <- TRUE
      }
      next
    }
    
    # --- Resto de ciudades ---
    y_pos <- ifelse(i %% 2 == 0, 1 - offset_total, 1 + offset_total)
    
    p <- p %>% add_annotations(
      x = df$comp[i],
      y = y_pos,
      text = ciudad_i,
      showarrow = FALSE,
      textangle = 90,
      font = list(size = 12, family = "Prompt", color = "#333333")
    )
  }
  
  
  # ---- Línea base ----
  p <- p %>% layout(
    title = NULL,
    xaxis = list(title = "", showticklabels = TRUE, showline = FALSE, zeroline = FALSE),
    yaxis = list(visible = FALSE, range = c(0.60, 1.40)),   # más espacio vertical
    shapes = list(
      list(
        type = "line", 
        x0 = 0, x1 = 0,
        y0 = 0, y1 = 2,
        line = list(color = "#E0E0E0", width = 1.5, dash = "dot")
      )
    )
  )
  
  # ---- Resumen extremos ----
  precio_max <- round(max(df$comp, na.rm = TRUE))
  precio_min <- round(min(df$comp, na.rm = TRUE))
  ciudad_max <- df$ciudad[which.max(df$comp)]
  ciudad_min <- df$ciudad[which.min(df$comp)]
  
  return(list(
    grafico    = p,
    datos      = df,
    precio_max = precio_max,
    precio_min = precio_min,
    ciudad_max = ciudad_max,
    ciudad_min = ciudad_min
  ))
}
