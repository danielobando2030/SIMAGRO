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
               glue,  tidyverse, gridExtra, corrplot)
options(scipen = 999)

###############################
##### Setting directories #####
###############################


###############################
##### Load and process data ###
###############################

data <- readRDS("precios_bogota_balanceado_3_8.rds") 

## LLenar con el promedio del valor anterior y el que sigue

data <- data %>%
  arrange(producto, mes_y_ano) %>%              # asegurar el orden
  group_by(producto) %>%                        # hacer por cada producto
  mutate(precio_prom = na.approx(precio_prom, na.rm = FALSE)) %>% 
  ungroup()

## Contar si hay missings por productos
missings <- data %>% group_by(producto) %>% summarise(n_missing = sum(is.na(precio_prom)))

fill_ma_both <- function(x, k = 12) {
  n <- length(x)
  
  # Hacia atrás (para NAs iniciales)
  for (i in seq(n, 1)) {
    if (is.na(x[i])) {
      future_vals <- x[(i+1):min(n, i+k)]
      future_vals <- future_vals[!is.na(future_vals)]
      if (length(future_vals) > 0) {
        x[i] <- mean(future_vals)
      }
    }
  }
  
  # Hacia adelante (para NAs finales)
  for (i in seq_len(n)) {
    if (is.na(x[i])) {
      past_vals <- x[max(1, i-k):(i-1)]
      past_vals <- past_vals[!is.na(past_vals)]
      if (length(past_vals) > 0) {
        x[i] <- mean(past_vals)
      }
    }
  }
  
  return(x)
}

# Aplicar por producto
data <- data %>%
  arrange(producto, mes_y_ano) %>%
  group_by(producto) %>%
  mutate(precio_prom = fill_ma_both(precio_prom, k = 12)) %>%
  ungroup()


## Contar si hay missings por productos

missings <- data %>% group_by(producto) %>% summarise(n_missing = sum(is.na(precio_prom)))

#### Termino de limpiar la base   
  
data <- data  %>% select(ciudad, producto, mes_y_ano, precio_prom)  

data$anio <- year(data$mes_y_ano)  
data$mes <- month(data$mes_y_ano)  
  

##### Funcion de correlación


correlacion_precios <- function(data, anio){
  
  # Filtrar por año
  df <- data %>%
    filter(anio == !!anio) %>%
    select(producto, mes_y_ano, precio_prom) %>%
    distinct()
  
  # Pasar a formato ancho
  df_wide <- df %>%
    pivot_wider(names_from = producto, values_from = precio_prom)
  
  # Quitar columna de fechas
  df_mat <- df_wide %>% select(-mes_y_ano)
  
  # Calcular matriz de correlaciones
  cor_mat <- cor(df_mat, use = "pairwise.complete.obs")
  
  # Nombres con mayúscula inicial
  productos <- str_to_title(colnames(cor_mat))
  colnames(cor_mat) <- productos
  rownames(cor_mat) <- productos
  
  # Crear gráfico interactivo con Plotly
  fig <- plot_ly(
    z = cor_mat,
    x = productos,
    y = productos,
    type = "heatmap",
    colorscale = list(
      list(0, "#8e44ad"),
      list(0.5, "white"),
      list(1, "#e74c3c")
    ),
    zmin = -1, zmax = 1,
    hovertemplate = paste(
      "<b>%{x}</b> vs <b>%{y}</b><br>",
      "Correlación: %{z:.2f}<extra></extra>"
    )
  ) %>%
    layout(
      title = list(text = paste("Matriz de correlación de precios de productos -", anio),
                   x = 0.5, font = list(size = 16)),
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "", autorange = "reversed")
    )
  
  return(list(
    matriz = cor_mat,
    grafico = fig
  ))
}

#correlacion_precios(data, 2023)

