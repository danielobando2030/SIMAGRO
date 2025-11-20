###############################
##### Libraries ############### 
############################### 

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl, glue, 
               tidyverse, gridExtra, corrplot, plotly, arrow) 
options(scipen = 999) 
options(encoding = "UTF-8")


data <- readRDS("base_elasticidad_3_3.rds") %>% ungroup()

#######

grafico_producto_anual <- function(data, producto_sel, anio_sel) {
  
  req_vars <- c("producto", "mes_y_ano", "suma_kg", "precio_prom", "elasticidad")
  faltan <- setdiff(req_vars, names(data))
  if (length(faltan) > 0) stop(paste("Faltan variables:", paste(faltan, collapse=", ")))
  
  df <- data %>%
    mutate(mes_y_ano = as.Date(mes_y_ano)) %>%
    filter(producto == producto_sel, year(mes_y_ano) == anio_sel)
  
  if (nrow(df) == 0) return(NULL)
  
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  df <- df %>%
    mutate(
      mes = month(mes_y_ano),
      mes_label = factor(meses_es[mes], levels = meses_es)
    )
  
  df_plot <- df %>%
    mutate(elasticidad = ifelse(is.na(elasticidad), 0, elasticidad))
  
  fig <- plot_ly(df_plot, x = ~mes, hovermode = "x unified") %>%
    add_lines(y = ~precio_prom, name = "Precio promedio",
              line = list(color="#494634", width=2, dash="dash")) %>%
    
    add_lines(y = ~suma_kg, name="Cantidades (kg)",
              line=list(color="#6D673E", width=2, dash="dashdot"), 
              yaxis="y2") %>%
    
    add_lines(y = ~elasticidad, name="Elasticidad",
              line=list(color="#DBC21F", width=2), yaxis="y3") %>%
    
    layout(
      xaxis = list(
        tickvals = 1:12,
        ticktext = meses_es
      ),
      
      yaxis = list(
        title = "",
        showticklabels = FALSE
      ),
      
      yaxis2 = list(
        title = "",
        overlaying = "y",
        side = "right",
        showticklabels = FALSE
      ),
      
      yaxis3 = list(
        title = "",
        overlaying = "y",
        side = "right",
        position = 1.05,
        showticklabels = FALSE
      )
    )
  
  return(fig)
}
# --- Ejemplo de uso ---
#grafico_producto_anual(data, "Aguacate", 2013)

