###############################
##### Libraries ###############
###############################

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl, glue, 
               tidyverse, gridExtra, corrplot, plotly, arrow) 
options(scipen = 999) 
options(encoding = "UTF-8")


data <- readRDS("base_elasticidad_3_3.rds") %>% ungroup()

###########################################################
### FUNCIÓN COMPLETA CON TOOLTIP NUEVO Y EJES VISIBLES  ###
###########################################################

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
      mes_label = meses_es[mes],
      elasticidad = ifelse(is.na(elasticidad), 0, elasticidad),
      suma_ton = suma_kg / 1000
    )
  
  # ================================
  # FORMATEOS LATAM CORREGIDOS
  # ================================
  
  precio_txt <- format(round(df$precio_prom, 0),
                       big.mark=".", decimal.mark=",")
  
  ton_txt <- format(round(df$suma_ton, 2),
                    nsmall = 2, big.mark=".", decimal.mark=",")
  
  elast_txt <- format(round(df$elasticidad, 2),
                      nsmall = 2, big.mark=".", decimal.mark=",")
  
  # ================================
  # GRÁFICO
  # ================================
  
  fig <- plot_ly(df, x = ~mes_label, hovermode = "x unified") %>%
    
    add_lines(
      y = ~precio_prom,
      name = "Precio promedio",
      line = list(color="#494634", width=2, dash="dash"),
      customdata = precio_txt,
      hovertemplate = paste0(
        "<b>Mes:</b> %{x}<br>",
        "<b>Precio promedio:</b> %{customdata}<br>",
        "<extra></extra>"
      )
    ) %>%
    
    add_lines(
      y = ~suma_ton,
      name = "Cantidad (t)",
      line = list(color="#6D673E", width=2, dash="dot"),
      yaxis = "y2",
      customdata = ton_txt,
      hovertemplate = paste0(
        "<b>Mes:</b> %{x}<br>",
        "<b>Cantidad (t):</b> %{customdata}<br>",
        "<extra></extra>"
      )
    ) %>%
    
    add_lines(
      y = ~elasticidad,
      name = "Elasticidad",
      line = list(color="#DBC21F", width=2),
      yaxis = "y3",
      customdata = elast_txt,
      hovertemplate = paste0(
        "<b>Mes:</b> %{x}<br>",
        "<b>Elasticidad:</b> %{customdata}<br>",
        "<extra></extra>"
      )
    ) %>%
    
    layout(
      xaxis = list(title = ""),
      
      yaxis = list(
        title = "Precio",
        showgrid = FALSE,
        showline = TRUE,
        linecolor = "#494634"
      ),
      
      yaxis2 = list(
        title = "Toneladas",
        overlaying = "y",
        side = "right",
        showline = TRUE,
        linecolor = "#6D673E"
      ),
      
      yaxis3 = list(
        title = list(text="Elasticidad", standoff=2),
        overlaying = "y",
        side = "right",
        position = 1.02,
        showline = TRUE,
        linecolor = "#DBC21F"
      ),
      
      legend = list(orientation = "h", y = -0.2),
      locale = "es"
    )
  
  return(fig)
}

