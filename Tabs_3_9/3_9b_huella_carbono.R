######
# Author: Luis Miguel García 
# Laura Quintero
# Daniel Obando 
# First Edited: 2025/08/24 
# Last Editor: 2025/11/10
# R version: 4.3.2 
###### 

############################### 
##### Libraries ############### 
############################### 
rm(list=ls()) 

pacman::p_load(readr, lubridate, dplyr, 
                             ggplot2, zoo, readxl, glue, 
                             tidyverse, gridExtra, corrplot, 
                             plotly, treemapify, arrow) 
options(scipen = 999) 

############################### 
##### Base #################### 
############################### 

data <- readRDS("datos_c02_3_9.rds") 
data <- data %>% group_by(categoria, anio, mes) %>% mutate( total_c02_grupo = sum(c02_total, na.rm = TRUE), 
                
peso_c02_grupo = ifelse( total_c02_grupo > 0, (c02_total / total_c02_grupo) * 100, 0 ) ) %>% ungroup() %>% mutate(peso_c02_grupo = round(peso_c02_grupo, 2))



##############################################
#### FUNCIÓN TREEMAP CO₂ — FORMATO LATAM  ####
##############################################

graficar_treemap_producto <- function(data, anio = NULL, mes = NULL) {
  
  # --- Filtrar datos ---
  df_filtrado <- data %>%
    {if (!is.null(anio)) filter(., anio == !!anio) else .} %>%
    {if (!is.null(mes)) filter(., mes == !!mes) else .}
  
  if (nrow(df_filtrado) == 0) return(NULL)
  
  df_filtrado <- df_filtrado %>%
    mutate(
      c02_total = as.numeric(c02_total),
      distancia = as.numeric(distancia),
      total_ton = as.numeric(total_ton),
      peso_c02_grupo = as.numeric(peso_c02_grupo)
    ) %>%
    filter(is.finite(c02_total), c02_total > 0)
  
  # --- Agregar por producto ---
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
      valor_fmt  = format(round(c02_total, 0), big.mark=".", decimal.mark=","),
      pct_fmt    = paste0(format(round(peso_c02_grupo_media, 1), big.mark=".", decimal.mark=","), "%"),
      texto_label = paste0(valor_fmt, "<br>", pct_fmt),
      
      hover_text = paste(
        "<b>Producto:</b>", producto, "<br>",
        "CO₂ Total: ", valor_fmt, " tCO₂<br>",
        "Distancia media: ", format(distancia_media, big.mark=".", decimal.mark=","), " km<br>",
        "Total transportado: ", format(total_ton_sum, big.mark=".", decimal.mark=","), " ton<br>",
        "Participación grupo: ", pct_fmt
      )
    )
  
  # Colores
  purples_scale <- colorRampPalette(
    c("#BC222A", "#983136", "#743639", "#592F30", "#3F2427", "#2B181A")
  )(500)
  
  # --- Categorías ---
  df_cat <- df_productos %>%
    group_by(categoria) %>%
    summarise(values = sum(c02_total), .groups = "drop") %>%
    mutate(
      labels = categoria,
      parents = "",
      texto_label = format(round(values, 0), big.mark=".", decimal.mark=","),
      hover_text = paste(
        "<b>Categoría:</b>", categoria, "<br>",
        "CO₂ Total: ", texto_label, " tCO₂"
      ),
      node_color = "#b0b0b0"
    )
  
  # --- Productos finales ---
  df_prod <- df_productos %>%
    mutate(
      labels  = producto,
      parents = categoria,
      values  = c02_total,
      node_color = purples_scale[
        floor(scales::rescale(c02_total, to=c(1,500)))
      ]
    )
  
  # --- Unión de nodos ---
  df_final <- bind_rows(
    df_cat %>% select(labels, parents, values, texto_label, hover_text, node_color),
    df_prod %>% select(labels, parents, values, texto_label, hover_text, node_color)
  )
  
  # --- Treemap ---
  p <- plot_ly(
    df_final,
    type = "treemap",
    labels = ~labels,
    parents = ~parents,
    values = ~values,
    text = ~texto_label,        # ← texto visible formateado
    hovertext = ~hover_text,
    hoverinfo = "text",
    marker = list(colors = ~node_color),
    textinfo = "label+text",    # ← mostramos lo que definimos
    branchvalues = "total"
  ) %>%
    layout(
      margin = list(t = 10, l = 0, r = 0, b = 0),
      uniformtext = list(minsize = 10, mode = "hide")
    )
  
  return(p)
}


##############################################
#### FUNCIÓN TREEMAP CO₂ — ESTÁTICO (GGPlot) ###
##############################################

graficar_treemap_producto_estatico <- function(data, anio = NULL, mes = NULL) {
  
  # --- 1. Filtrar y Procesar datos ---
  df_filtrado <- data %>%
    {if (!is.null(anio)) filter(., anio == !!anio) else .} %>%
    {if (!is.null(mes))  filter(., mes  == !!mes)  else .}
  
  if (nrow(df_filtrado) == 0) return(ggplot() + theme_void())
  
  df_prod <- df_filtrado %>%
    mutate(c02_total = as.numeric(c02_total), peso_c02_grupo = as.numeric(peso_c02_grupo)) %>%
    filter(is.finite(c02_total), c02_total > 0) %>%
    group_by(categoria, producto) %>%
    summarise(
      c02_total = sum(c02_total, na.rm = TRUE),
      peso_c02_grupo = mean(peso_c02_grupo, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      valor_fmt = format(round(c02_total, 0), big.mark=".", decimal.mark=","),
      pct_fmt   = paste0(format(round(peso_c02_grupo, 1), big.mark=".", decimal.mark=","), "%"),
      label     = paste0(producto, "\n", valor_fmt, "\n", pct_fmt)
    )
  
  # --- 2. Gráfico Estático con Jerarquía Gris Claro ---
  ggplot(df_prod, aes(area = c02_total, subgroup = categoria)) +
    
    # Capa 1: Fondo del Contenedor (Gris muy tenue para el encasillado)
    treemapify::geom_treemap_subgroup_border(fill = "#EDEDED", color = "white", linewidth = 3) +
    
    # Capa 2: Alimentos (Rojos)
    treemapify::geom_treemap(
      aes(fill = c02_total), 
      color = "white", 
      linewidth = 0.7
    ) +
    
    # Capa 3: Nombre del Grupo (CENTRO, GRIS CLARO, ENCIMA)
    treemapify::geom_treemap_subgroup_text(
      place = "centre", 
      colour = "#A0A0A0", # Gris más claro y suave
      alpha = 0.7,        # Transparencia ajustada para suavidad
      grow = FALSE,       
      size = 11,          
      fontface = "bold",
      min.size = 0
    ) +
    
    # Capa 4: Etiquetas de Alimentos (Blanco)
    treemapify::geom_treemap_text(
      aes(label = label),
      colour = "white", 
      place = "centre", 
      size = 8,
      grow = FALSE, 
      reflow = TRUE
    ) +
    
    # Escala de rojos consistente
    scale_fill_gradientn(
      colours = c("#BC222A", "#983136", "#743639", "#592F30", "#3F2427", "#2B181A"),
      guide = "none"
    ) +
    
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))
}
#################################
# PRUEBA
#################################
# graficar_treemap_producto(data, anio = 2024, mes = 12)
