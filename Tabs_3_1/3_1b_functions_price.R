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
graficar_variable <- function(temporalidad = c("mensual", "diaria"),
                              alimento = NULL,
                              variable = "cambio_pct",
                              anio_filtro = NULL) {

temporalidad <- match.arg(temporalidad)

# Seleccionar dataset
df_graf <- if (temporalidad == "mensual") mensual else diaria

# Estandarizar formato
df_graf <- df_graf %>%
  mutate(
    producto = str_to_title(producto),
    mes_y_ano = as.Date(mes_y_ano)
  )

# Filtros
if (!is.null(alimento) && alimento != "") {
  df_graf <- df_graf %>% filter(producto == str_to_title(alimento))
}
if (!is.null(anio_filtro) && anio_filtro != "") {
  df_graf <- df_graf %>% filter(anio == anio_filtro)
}

# Validación
if (nrow(df_graf) == 0 || !variable %in% names(df_graf)) {
  stop("⚠️ No hay datos disponibles para los filtros seleccionados o la variable no existe.")
}

# Tooltip
formato_fecha <- if (temporalidad == "mensual") "%b-%Y" else "%d-%b-%Y"
df_graf <- df_graf %>%
  mutate(
    etiqueta = paste0(
      "<b>Fecha:</b> ", format(mes_y_ano, formato_fecha),
      "<br><b>Producto:</b> ", producto,
      "<br><b>", str_to_title(variable), ":</b> ", format(round(.data[[variable]], 2), big.mark = ",")
    )
  )

# Gráfico interactivo con plotly (sin ggplotly)
grafico_interactivo <- plot_ly(
  data = df_graf,
  x = ~mes_y_ano,
  y = as.formula(paste0("~", variable)),
  type = "scatter",
  mode = "lines+markers",
  text = ~etiqueta,
  hoverinfo = "text",
  line = list(color = "#6A0DAD", width = 2),
  marker = list(color = "#6A0DAD", size = 6)
) %>%
  layout(
    xaxis = list(title = "Fecha"),
    yaxis = list(title = str_to_title(variable)),
    hoverlabel = list(bgcolor = "white", font = list(color = "black")),
    hovermode = "x unified"
  )

list(
  grafico = grafico_interactivo,
  datos = df_graf,
  promedio = mean(df_graf[[variable]], na.rm = TRUE)
)
}



# Mensual
graficar_variable(temporalidad = "mensual", alimento = "aguacate", variable = "precio_prom", anio_filtro = 2023)

# Diario
graficar_variable(temporalidad = "diaria", alimento = "aguacate", variable = "cambio_pct", anio_filtro = 2014)


graficar_variable <- function(temporalidad = c("mensual", "diaria"),
                              alimento = NULL,
                              variable = "cambio_pct",
                              anio_filtro = NULL) {
  
  temporalidad <- match.arg(temporalidad)
  
  # Seleccionar dataset
  df_graf <- if (temporalidad == "mensual") mensual else diaria
  
  # Estandarizar formato
  df_graf <- df_graf %>%
    mutate(
      producto = str_to_title(producto),
      mes_y_ano = as.Date(mes_y_ano)
    )
  
  # Filtros
  if (!is.null(alimento) && alimento != "") {
    df_graf <- df_graf %>% filter(producto == str_to_title(alimento))
  }
  if (!is.null(anio_filtro) && anio_filtro != "") {
    df_graf <- df_graf %>% filter(anio == anio_filtro)
  }
  
  # Validación
  if (nrow(df_graf) == 0 || !variable %in% names(df_graf)) {
    stop("⚠️ No hay datos disponibles para los filtros seleccionados o la variable no existe.")
  }
  
  # Tooltip
  formato_fecha <- if (temporalidad == "mensual") "%b-%Y" else "%d-%b-%Y"
  df_graf <- df_graf %>%
    mutate(
      etiqueta = paste0(
        "<b>Fecha:</b> ", format(mes_y_ano, formato_fecha),
        "<br><b>Producto:</b> ", producto,
        "<br><b>", str_to_title(variable), ":</b> ", format(round(.data[[variable]], 2), big.mark = ",")
      )
    )
  
  # Gráfico plano (ggplot)
  grafico_plano <- ggplot(df_graf, aes(x = mes_y_ano, y = .data[[variable]])) +
    geom_line(color = "#6A0DAD", linewidth = 1) +
    geom_point(color = "#6A0DAD", size = 2) +
    labs(x = "Fecha", y = str_to_title(variable)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  # Gráfico interactivo
  grafico_interactivo <- plot_ly(
    data = df_graf,
    x = ~mes_y_ano,
    y = as.formula(paste0("~", variable)),
    type = "scatter",
    mode = "lines+markers",
    text = ~etiqueta,
    hoverinfo = "text",
    line = list(color = "#6A0DAD", width = 2),
    marker = list(color = "#6A0DAD", size = 6)
  ) %>%
    layout(
      xaxis = list(title = "Fecha"),
      yaxis = list(title = str_to_title(variable)),
      hoverlabel = list(bgcolor = "white", font = list(color = "black")),
      hovermode = "x unified"
    )
  
  # Calcular mínimos
  idx_min <- which.min(df_graf[[variable]])
  min_valor <- round(df_graf[[variable]][idx_min], 2)
  fecha_min_valor <- df_graf$mes_y_ano[idx_min]
  producto_min_valor <- ifelse("producto" %in% names(df_graf), df_graf$producto[idx_min], NA)
  
  # Retornar outputs al estilo FAO
  return(list(
    grafico = grafico_interactivo,
    grafico_plano = grafico_plano,
    datos = df_graf,
    min_valor = min_valor,
    fecha_min_valor = fecha_min_valor,
    producto_min_valor = producto_min_valor
  ))
}







