######
# Author:       Luis Miguel García
#               Daniel Obando
# First Edited: 2025/08/24
# Last Editor:  2025/09/12
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
##### Setting directories #####
###############################

##############################

base_precios <- readRDS("data_precios_insumos_3_10.rds") 


# ---- Función ----
boxplot_interactivo <- function(data, presentacion_sel, subgrupo_sel) {
  
  # ============================================================
  # 1. Selección y filtrado del dataset
  # ============================================================
  data_filtrada <- data %>%
    filter(
      presentacion == presentacion_sel,
      subgrupos == subgrupo_sel
    )
  
  if (nrow(data_filtrada) == 0) {
    stop("No hay datos para esa combinación de presentación y subgrupo.")
  }
  
  all_years <- sort(unique(data$anio))
  
  
  # ============================================================
  # 2. Limpieza y transformación (análogo a Caja_y_Bigotes)
  # ============================================================
  data_filtrada$anio <- as.factor(data_filtrada$anio)
  
  data_filtrada <- data_filtrada %>% 
    select(anio, municipio, precio)
  
  data_filtrada <- data_filtrada[rowSums(is.na(data_filtrada)) == 0, ]
  
  
  # ============================================================
  # 3. Estadísticas resumidas (igual estructura que "resumen")
  # ============================================================
  resumen <- data_filtrada %>%
    group_by(anio) %>%
    summarise(
      n = n(),
      mediana = median(precio, na.rm = TRUE),
      q1 = quantile(precio, 0.25, na.rm = TRUE),
      q3 = quantile(precio, 0.75, na.rm = TRUE),
      minimo = min(precio, na.rm = TRUE),
      maximo = max(precio, na.rm = TRUE),
      municipio_min = municipio[which.min(precio)],
      municipio_max = municipio[which.max(precio)]
    )
  
  
  # ============================================================
  # 4. Unión de estadísticas al dataset
  # ============================================================
  BD <- data_filtrada %>%
    left_join(resumen, by = "anio")
  
  
  # ============================================================
  # 5. Tooltip interactivo
  # ============================================================
  BD$tooltip_text <- paste0(
    "<b>Año:</b> ", BD$anio, "<br>",
    "<b>Precio:</b> ", dollar(BD$precio, big.mark = ".", decimal.mark = ","), "<br>",
    "<b>Mediana:</b> ", dollar(round(BD$mediana, 0), big.mark = ".", decimal.mark = ","), "<br>",
    "<b>Q1:</b> ", dollar(round(BD$q1, 0), big.mark = ".", decimal.mark = ","), "<br>",
    "<b>Q3:</b> ", dollar(round(BD$q3, 0), big.mark = ".", decimal.mark = ","), "<br>",
    "<b>Mínimo:</b> ", dollar(round(BD$minimo, 0), big.mark = ".", decimal.mark = ","), 
    " (", BD$municipio_min, ")<br>",
    "<b>Máximo:</b> ", dollar(round(BD$maximo, 0), big.mark = ".", decimal.mark = ","), 
    " (", BD$municipio_max, ")<br>",
    "<b>Número de observaciones:</b> ", BD$n
  )
  
  
  # ============================================================
  # 6. Gráfico base (ggplot) — mismo estilo que Caja_y_Bigotes
  # ============================================================
  graf <- ggplot(
    BD, 
    aes(
      x = factor(anio, levels = all_years),
      y = precio,
      fill = factor(anio, levels = all_years)
    )
  ) +
    geom_violin(color = "black", alpha = 0.8, width = 1, show.legend = FALSE) +
    geom_boxplot(width = 0.25, color = "black", alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(text = tooltip_text), size = 1, color = "gray40", alpha = 0.4, width = 0.15, show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        "2013" = "#E4C6C8",
        "2014" = "#D9A8AB",
        "2015" = "#CE8B8F",
        "2016" = "#C26E73",
        "2017" = "#B65258",
        "2018" = "#A73E44",
        "2019" = "#9A373D",
        "2020" = "#8C3136",
        "2021" = "#7F2B30",
        "2022" = "#742E33",
        "2023" = "#66302F",
        "2024" = "#592C2B",
        "2025" = "#4D2927"
      )
    ) +
    labs(fill = "Año", y = "Precio", x = "") +
    theme_minimal(base_size = 13) 
  
  
  # ============================================================
  # 7. Versión interactiva con plotly (igual estructura)
  # ============================================================
  graf_plotly <- plotly::ggplotly(graf+theme(
    #axis.text.x = element_blank(),
    #axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)), tooltip = "text") %>%
    plotly::layout(
      hoverlabel = list(bgcolor = "white", font = list(size = 12))
    )
  
  
  # ============================================================
  # 8. Texto interpretativo (siguiendo tu mismo estilo)
  # ============================================================
  ultimo_anio <- max(as.numeric(as.character(BD$anio)))
  S <- BD[BD$anio == ultimo_anio, ][1, ]

  Text_ <- paste(
    "En el año", ultimo_anio,
    "el 25% de los municipios registraron precios de los ",subgrupo_sel," en presentación de ",presentacion_sel," menores o iguales a",
    dollar(S$q1, decimal.mark = ",", big.mark = "."),
    "; el 50% registraron valores menores o iguales a",
    dollar(S$mediana, decimal.mark = ",", big.mark = "."),
    "; y el 75% alcanzaron precios menores o iguales a",
    dollar(S$q3, decimal.mark = ",", big.mark = ".")
  )
  
  
  # ============================================================
  # 9. Retorno — MISMO FORMATO QUE Caja_y_Bigotes
  # ============================================================
  return(list(
    grafico_plano = graf,
    grafico_plotly = graf_plotly,
    datos = BD,
    Text_ = Text_,
    resumen = resumen
  ))
}

#boxplot_interactivo(
#  data = base_precios,
#  presentacion_sel = "1 litro",
#  subgrupo_sel = "Fungicidas"
#)
