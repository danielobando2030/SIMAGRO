######
# Author:       Luis Miguel García
#               Laura Quintero
#               Daniel Obando
# First Edited: 2025/08/24
# Last Editor:  2025/09/12
# R version:    4.3.2
######


###############################
##### Libraries ###############
###############################

pacman::p_load(readr, lubridate, dplyr, ggplot2, zoo, readxl,
               glue,  tidyverse, gridExtra, corrplot, forcats)
options(scipen = 999)

###############################
##### Setting directories #####
###############################


data <- readRDS("base_precios_mayorista_mes_filtrados_3_5.rds") 

####

visualizar_ranking <- function(data, producto, anio) {
  # preparar y validar
  data_f <- data %>%
    mutate(mes_y_ano = as.yearmon(mes_y_ano, "%Y-%m")) %>%
    filter(producto == !!producto,
           format(mes_y_ano, "%Y") == anio) %>%
    filter(!is.na(precio_prom))
  
  if (nrow(data_f) == 0) stop(paste("No hay datos para producto =", producto, "y año =", anio))
  
  # ranking mensual
  if (!("ranking" %in% names(data_f)) | !("total_ciudades" %in% names(data_f))) {
    data_f <- data_f %>%
      group_by(producto, mes_y_ano) %>%
      mutate(
        ranking = rank(-precio_prom, ties.method = "min"),
        total_ciudades = n()
      ) %>%
      ungroup()
  }
  
  # meses en español
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  data_f <- data_f %>%
    mutate(
      mes = month(mes_y_ano),
      mes_label = factor(meses_es[mes], levels = meses_es),
      hover_text = paste0(
        "<b>Ciudad:</b> ", ciudad, "<br>",
        "<b>Mes:</b> ", mes_label, "<br>",
        "<b>Precio promedio:</b> $", format(precio_prom, big.mark = ",", scientific = FALSE), "<br>",
        "<b>Puesto de ", ciudad, ":</b> ", ranking, "<br>",
        "<b>Ciudades:</b> ", total_ciudades
      )
    )
  
  # separar Bogotá y las demás
  data_bogota <- filter(data_f, ciudad == "Bogotá")
  data_otros <- filter(data_f, ciudad != "Bogotá")
  
  # gráfico interactivo
  fig <- plot_ly()
  
  # 1️⃣ Otros
  fig <- fig %>%
    add_trace(
      data = data_otros,
      x = ~mes_label,
      y = ~precio_prom,
      type = 'scatter',
      mode = 'markers+text',
      text = ~ciudad,
      textposition = 'right middle',
      textfont = list(size = 7, color = "#B0B0B0"),
      hoverinfo = 'text',
      hovertext = ~hover_text,
      marker = list(size = 8, color = "#B0B0B0", line = list(width = 1, color = "white")),
      showlegend = FALSE
    )
  
  # 2️⃣ Bogotá al frente
  fig <- fig %>%
    add_trace(
      data = data_bogota,
      x = ~mes_label,
      y = ~precio_prom,
      type = 'scatter',
      mode = 'markers+text',
      text = ~ciudad,
      textposition = 'right middle',
      textfont = list(size = 10, color = "#7B1FA2"),
      hoverinfo = 'text',
      hovertext = ~hover_text,
      marker = list(size = 10, color = "#7B1FA2", line = list(width = 3, color = "gold")),
      showlegend = FALSE
    )
  
  fig <- fig %>%
    layout(
      title = NULL,  
      xaxis = list(title = "Mes", tickangle = -45, showgrid = FALSE),
      yaxis = list(title = "Precio promedio por Kg"),
      hoverlabel = list(bgcolor = "white", font = list(size = 12))
    )
  
  return(fig)
}

#visualizar_ranking(data, producto = "Habichuela", anio = "2015")
