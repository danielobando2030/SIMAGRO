######
# Author:       Luis Miguel García
#               Laura Quintero
#               Daniel Obando
# First Edited: 2025/08/24
# Last Editor:  2025/11/10
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
##### Base ####################
###############################

data <- readRDS("datos_c02_3_9.rds")

data <- data %>%
  group_by(categoria, anio, mes) %>%
  mutate(
    total_c02_grupo = sum(c02_total, na.rm = TRUE),
    peso_c02_grupo = ifelse(
      total_c02_grupo > 0,
      (c02_total / total_c02_grupo) * 100,
      0
    )
  ) %>%
  ungroup() %>%
  mutate(peso_c02_grupo = round(peso_c02_grupo, 2))

#######################################
##### Función de visualización ########
#######################################

graficar_treemap_producto <- function(data, anio = NULL, mes = NULL) {
  
  # --- 1. Filtrado y Limpieza de datos ---
  df_filtrado <- data %>%
    {if (!is.null(anio)) dplyr::filter(., anio == !!anio) else .} %>%
    {if (!is.null(mes)) dplyr::filter(., mes == !!mes) else .}
  
  if (nrow(df_filtrado) == 0) {
    message("¡Advertencia! No hay datos para el año y/o mes(es) seleccionado(s).")
    return(NULL)
  }
  
  df_filtrado <- df_filtrado %>%
    mutate(
      c02_total = as.numeric(c02_total),
      distancia = as.numeric(distancia),
      total_ton = as.numeric(total_ton),
      peso_c02_grupo = as.numeric(peso_c02_grupo)
    ) %>%
    filter(is.finite(c02_total), c02_total > 0)
  
  # --- 2. Agregación y etiquetas hover ---
  df_productos <- df_filtrado %>%
    group_by(categoria, producto) %>%
    summarise(
      c02_total = sum(c02_total, na.rm = TRUE),
      distancia_media = mean(distancia, na.rm = TRUE),
      total_ton_sum = sum(total_ton, na.rm = TRUE),
      peso_c02_grupo_media = mean(peso_c02_grupo, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      CO2_str = paste0(round(c02_total, 0), " tCO₂"),
      distancia_str = paste0(round(distancia_media, 0), " km"),
      total_ton_str = paste0(round(total_ton_sum, 2), " ton"),
      peso_c02_grupo_str = paste0(round(peso_c02_grupo_media, 2), "%"),
      hover_text = paste(
        "<b>Producto:</b>", producto, "<br>",
        "CO₂ Total:", CO2_str, "<br>",
        "Distancia media:", distancia_str, "<br>",
        "Total transportado:", total_ton_str, "<br>",
        "Participación grupo:", peso_c02_grupo_str
      )
    )
  
  # --- 3. Escalas de color ---
  # Más granularidad y mejor transición perceptual
  purples_scale <- colorRampPalette(
    c(
      "#BC222A",  # 1. rojo profundo fuerte
      "#983136",  # 2. vino saturado
      "#743639",  # 3. vino oscuro
      "#592F30",  # 4. vino-marrón fuerte
      "#3F2427",  # 5. vino-marrón oscuro
      "#2B181A"   # 6. vino-marrón muy oscuro
    )
  )(500)
  
  blues_scale <- colorRampPalette(
    c("#f2f2f2", "#d9d9d9", "#bdbdbd", "#969696", "#525252")
  )(200)
  
  # --- 4. Categorías (nivel padre) ---
  df_categorias <- df_productos %>%
    group_by(categoria) %>%
    summarise(
      values = sum(c02_total, na.rm = TRUE),
      distancia_str = paste0(round(mean(distancia_media, na.rm = TRUE), 0), " km"),
      total_ton_str = paste0(round(sum(total_ton_sum, na.rm = TRUE), 2), " ton"),
      peso_c02_grupo_media = mean(peso_c02_grupo_media, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      labels = categoria,
      parents = "",
      CO2_str = paste0(round(values, 0), " tCO₂"),
      peso_c02_grupo_str = paste0(round(peso_c02_grupo_media, 2), "%"),
      hover_text = paste(
        "<b>Categoría:</b>", categoria, "<br>",
        "CO₂ Total (suma):", CO2_str, "<br>",
        "Distancia media:", distancia_str, "<br>",
        "Total alimento:", total_ton_str, "<br>",
        "Peso grupo (media):", peso_c02_grupo_str
      )
    ) %>%
    ungroup() %>%
    mutate(
      scaled_values = (values - min(values, na.rm = TRUE)) /
        (max(values, na.rm = TRUE) - min(values, na.rm = TRUE)),
      scaled_values = ifelse(is.na(scaled_values), 0, scaled_values),
      color_index = floor(scaled_values * 199) + 1,
      node_color = blues_scale[color_index]
    ) %>%
    select(labels, parents, values, hover_text, node_color)
  
  # --- 5. Productos (nivel hijo) ---
  df_productos_final <- df_productos %>%
    mutate(
      labels = producto,
      parents = categoria,
      values = c02_total
    ) %>%
    ungroup() %>%
    mutate(
      scaled_values = (values - min(values, na.rm = TRUE)) /
        (max(values, na.rm = TRUE) - min(values, na.rm = TRUE)),
      scaled_values = ifelse(is.na(scaled_values), 0, scaled_values),
      # ^0.6 da mayor contraste visual a los productos de alta emisión
      color_index = floor((scaled_values ^ 0.6) * 499) + 1,
      node_color = purples_scale[pmin(color_index, 500)]
    ) %>%
    select(labels, parents, values, hover_text, node_color)
  
  # --- 6. Unión de niveles ---
  df_final <- bind_rows(df_categorias, df_productos_final)
  
  # --- 7. Treemap final (SIN TÍTULO) ---
  p <- plot_ly(
    df_final,
    type = "treemap",
    labels = ~labels,
    parents = ~parents,
    values = ~values,
    marker = list(colors = ~node_color),
    text = ~hover_text,
    hoverinfo = 'text',
    branchvalues = "total",
    textinfo = "label+value+percent parent"
  )
  
  p <- p %>%
    layout(
      title = list(text = ""),   # ← TÍTULO VACÍO (eliminado)
      uniformtext = list(minsize = 10, mode = 'hide'),
      margin = list(t = 10, l = 0, r = 0, b = 0)   # ← reduce espacio superior
    )
  
  return(p)
}

#######################################
##### Ejemplo de prueba ##############
#######################################

graficar_treemap_producto(data, anio = 2013, mes = 1)
