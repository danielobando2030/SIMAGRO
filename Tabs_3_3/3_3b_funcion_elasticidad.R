###############################
##### Libraries ############### 
############################### 

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl, glue, 
               tidyverse, gridExtra, corrplot, plotly, arrow) 
options(scipen = 999) 
options(encoding = "UTF-8")


data <- readRDS("base_elasticidad_3_3.rds")


grafico_producto_anual <- function(data, producto_sel, anio_sel) {
  
  # Validar variables requeridas
  req_vars <- c("producto", "mes_y_ano", "suma_kg", "precio_prom", "elasticidad")
  faltan <- setdiff(req_vars, names(data))
  if (length(faltan) > 0) stop(paste("Faltan las variables:", paste(faltan, collapse = ", ")))
  
  # Filtrar datos por producto y año
  df <- data %>%
    filter(producto == producto_sel, year(mes_y_ano) == anio_sel)
  
  if (nrow(df) == 0) {
    warning("No hay datos para ese producto y año.")
    return(NULL)
  }
  
  # Meses en español
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  df <- df %>%
    mutate(mes = month(mes_y_ano),
           mes_label = factor(meses_es[mes], levels = meses_es))
  
  # Gráfico con hover unificado
  fig <- plot_ly(df, x = ~mes_label, hovermode = "x unified") %>%
    
    # Precio promedio (línea negra discontinua)
    add_lines(y = ~precio_prom, name = "Precio promedio",
              line = list(color = "#000000", width = 2, dash = "dash"),
              hovertemplate = paste(
                "<b>Precio:</b> $%{y:,.0f}<extra></extra>"
              )) %>%
    
    # Cantidades (azul rey discontinua)
    add_lines(y = ~suma_kg, name = "Cantidades (kg)",
              line = list(color = "#0057B8", width = 2, dash = "dash"),
              hovertemplate = paste(
                "<b>Cantidades:</b> %{y:,.0f} kg<extra></extra>"
              ),
              yaxis = "y2") %>%
    
    # Elasticidad (púrpura continua)
    add_lines(y = ~elasticidad, name = "Elasticidad",
              line = list(color = "#800080", width = 2, dash = "solid"),
              hovertemplate = paste(
                "<b>Elasticidad:</b> %{y:.2f}<extra></extra>"
              ),
              yaxis = "y3") %>%
    
    layout(
      title = NULL,
      xaxis = list(
        title = "Mes",
        tickvals = meses_es,
        ticktext = meses_es,
        showgrid = FALSE,
        tickfont = list(size = 12, color = "black")
      ),
      yaxis = list(
        title = list(text = ""),  
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        title = list(text = ""),  
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      yaxis3 = list(
        overlaying = "y",
        side = "right",
        position = 1.05,
        title = list(text = ""),  
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      legend = list(
        orientation = "h",
        x = 0.3, y = -0.25
      ),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      ),
      hovermode = "x unified"
    )
  
  return(fig)
}

# --- Ejemplo de uso ---
#grafico_producto_anual(data, "Aguacate", 2013)

