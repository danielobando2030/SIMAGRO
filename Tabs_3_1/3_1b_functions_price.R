###### # Author: Luis Miguel GarcC-a 
# Laura Quintero 
# Daniel Obando 
# First Edited: 2025/08/24 
# Last Editor: 2025/09/12 
# R version: 4.3.2 
###### 

if (interactive()) 
{ rm(list=ls()) } 

###############################
##### Libraries ############### 

############################### 

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl, glue, 
               tidyverse, gridExtra, corrplot, plotly, shiny) 
options(scipen = 999) 
options(encoding = "UTF-8")

############################### 
##### Setting directories #####
############################### 

############################### ##### Load and process mensual #### ############################### 
mensual <- readRDS("base_mensual_mayoristas_indices_bog_3_1.rds") 
mensual <- mensual %>% mutate(across(where(is.character), ~ enc2utf8(.)))
mensual$anio <- year(mensual$mes_y_ano)
#
diaria <- readRDS("base_diaria_mayoristas_indices_bog_3_1.rds") 

names(diaria)[names(diaria) == "fecha"] <- "mes_y_ano"

diaria <- diaria %>% mutate(across(where(is.character), ~ enc2utf8(.)))

diaria$anio <- year(diaria$mes_y_ano)

##########
# ============================================================
# PALETA INSTITUCIONAL FAO–VP
# ============================================================
col_palette <-c("#DBC21F", "#B6A534", "#6D673E", "#494634")
# Primer color (líneas y puntos)
col_grafico <- col_palette[1]

# ============================================================
# FUNCIÓN graficar_variable()
# ============================================================
graficar_variable <- function(temporalidad = c("mensual", "diaria"),
                              alimento = NULL,
                              variable = "cambio_pct",
                              anio_filtro = NULL) {
  
  temporalidad <- match.arg(temporalidad)
  
  # -----------------------------
  # 1. Seleccionar dataset
  # -----------------------------
  df_graf <- if (temporalidad == "mensual") mensual else diaria
  
  # -----------------------------
  # 2. Estandarizar formato
  # -----------------------------
  df_graf <- df_graf %>%
    mutate(
      producto = str_to_title(producto),
      mes_y_ano = as.Date(mes_y_ano)
    )
  
  # -----------------------------
  # 3. Filtros
  # -----------------------------
  if (!is.null(alimento) && alimento != "") {
    df_graf <- df_graf %>% filter(producto == str_to_title(alimento))
  }
  if (!is.null(anio_filtro) && anio_filtro != "") {
    df_graf <- df_graf %>% filter(anio == anio_filtro)
  }
  
  # -----------------------------
  # 4. Validación
  # -----------------------------
  if (nrow(df_graf) == 0 || !variable %in% names(df_graf)) {
    stop("⚠️ No hay datos disponibles para los filtros seleccionados o la variable no existe.")
  }
  
  # -----------------------------
  # 5. Tooltip Plotly
  # -----------------------------
  formato_fecha <- if (temporalidad == "mensual") "%b-%Y" else "%d-%b-%Y"
  
  df_graf <- df_graf %>%
    mutate(
      etiqueta = paste0(
        "<b>Fecha:</b> ", format(mes_y_ano, formato_fecha),
        "<br><b>Producto:</b> ", producto,
        "<br><b>", str_to_title(variable), ":</b> ",
        format(round(.data[[variable]], 2), big.mark = ",")
      )
    )
  
  # -----------------------------
  # 6. GRÁFICO PLANO (ggplot)
  # -----------------------------
  grafico_plano <- ggplot(df_graf, aes(x = mes_y_ano, y = .data[[variable]])) +
    geom_line(color = col_grafico, linewidth = 1.4) +
    geom_point(color = col_grafico, size = 2.8) +
    labs(x = "Fecha", y = str_to_title(variable)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  # -----------------------------
  # 7. GRÁFICO INTERACTIVO (plotly)
  # -----------------------------
  grafico_interactivo <- plot_ly(
    data = df_graf,
    x = ~mes_y_ano,
    y = as.formula(paste0("~", variable)),
    type = "scatter",
    mode = "lines+markers",
    text = ~etiqueta,
    hoverinfo = "text",
    line = list(color = col_grafico, width = 2.5),
    marker = list(color = col_grafico, size = 6)
  ) %>%
    layout(
      xaxis = list(title = "Fecha"),
      yaxis = list(title = str_to_title(variable)),
      hoverlabel = list(bgcolor = "white", font = list(color = "black")),
      hovermode = "x unified"
    )
  
  # -----------------------------
  # 8. Cálculo de mínimo
  # -----------------------------
  idx_min <- which.min(df_graf[[variable]])
  min_valor <- round(df_graf[[variable]][idx_min], 2)
  fecha_min_valor <- df_graf$mes_y_ano[idx_min]
  producto_min_valor <- df_graf$producto[idx_min]
  
  # -----------------------------
  # 9. Retornar lista FAO
  # -----------------------------
  return(list(
    grafico = grafico_interactivo,
    grafico_plano = grafico_plano,
    datos = df_graf,
    min_valor = min_valor,
    fecha_min_valor = fecha_min_valor,
    producto_min_valor = producto_min_valor
  ))
}






