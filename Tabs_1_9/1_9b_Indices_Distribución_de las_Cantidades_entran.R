#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Cristian Daniel Obando Arbeláez, Luis Miguel García
#Fecha de creacion: 24/09/2025
#Fecha de ultima modificacion: 16/11/2025
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(arrow)
options(scipen = 999)
################################################################################-


abastecimiento_bogota_ANO_mes_alimento_completo=readRDS("abastecimiento_bogota_ANO_mes_alimento_completo_1_9.RDS")
abastecimiento_bogota_ANO_mes_total_completo=readRDS("abastecimiento_bogota_ANO_mes_total_completo_1_9.RDS")

filt1=!(abastecimiento_bogota_ANO_mes_alimento_completo$anio==2025&abastecimiento_bogota_ANO_mes_alimento_completo$mes==9)
filt2=!(abastecimiento_bogota_ANO_mes_total_completo$anio==2025&abastecimiento_bogota_ANO_mes_total_completo$mes==9)
abastecimiento_bogota_ANO_mes_alimento_completo=abastecimiento_bogota_ANO_mes_alimento_completo[filt1,]
abastecimiento_bogota_ANO_mes_total_completo=abastecimiento_bogota_ANO_mes_total_completo[filt2,]

Caja_y_Bigotes = function(ALIMENTO = NULL, formato = "numeric") {
  # --- 1. Selección del dataset ---
  if (!is.null(ALIMENTO)) {
    BD <- subset(abastecimiento_bogota_ANO_mes_alimento_completo, producto == ALIMENTO)
  } else {
    BD <- abastecimiento_bogota_ANO_mes_total_completo
  }
  
  # --- 2. Limpieza y transformación ---
  BD$mes <- months(as.Date(paste0("2025-", BD$mes, "-01")))  
  BD$suma_kg <- as.numeric(BD$suma_kg)
  BD$anio <- as.factor(BD$anio)
  BD <- BD[, c("anio", "suma_kg")]
  names(BD) <- c("var1", "value")
  BD <- BD[rowSums(is.na(BD)) == 0, ]
  
  # --- 3. Estadísticas resumidas ---
  resumen <- BD %>%
    group_by(var1) %>%
    summarise(
      mediana = median(value, na.rm = TRUE),
      q1 = quantile(value, 0.25, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      promedio = mean(value, na.rm = TRUE)
    )
  
  BD <- left_join(BD, resumen, by = "var1")
  
  # --- 4. Función para formato de texto ---
  f1 <- function(x, formato) {
    if (formato == "percent") {
      return(scales::percent(x, accuracy = 0.1, big.mark = ".", decimal.mark = ","))
    } else if (formato == "dollar") {
      return(scales::dollar(x, big.mark = ".", decimal.mark = ","))
    } else {
      return(format(round(x), big.mark = ".", small.mark = ","))
    }
  }
  
  # --- 5. Tooltip interactivo ---
  BD$tooltip_text <- paste0(
    "<b>Año:</b> ", BD$var1,
    "<br><b>Cantidad:</b> ", f1(BD$value, formato),
    "<br><b>Mediana:</b> ", f1(BD$mediana, formato),
    "<br><b>Q1:</b> ", f1(BD$q1, formato),
    "<br><b>Q3:</b> ", f1(BD$q3, formato),
    "<br><b>Promedio:</b> ", f1(BD$promedio, formato)
  )
  
  # --- 6. Gráfico base ggplot ---
  graf <-  ggplot(BD, aes(x = var1, y = value, fill = var1)) +
    geom_violin(color = "black", alpha = 0.8, width = 1, show.legend = FALSE) +
    geom_boxplot(width = 0.25, color = "black", alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(text = tooltip_text), size = 1, color = "gray40", alpha = 0.4, width = 0.15, show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        "2013" = "#0087CF",
        "2014" = "#007AB8",
        "2015" = "#006EA2",
        "2016" = "#00628C",
        "2017" = "#005776",
        "2018" = "#004C61",
        "2019" = "#00414D",
        "2020" = "#00363A",
        "2021" = "#002C28",
        "2022" = "#66B7E0",
        "2023" = "#4DAADD",
        "2024" = "#339DD9",
        "2025" = "#1A91D5"
      )
      
    ) +
    labs(fill = "Año", y = "Kilogramos",x="") +
    theme_minimal(base_size = 13)+scale_y_continuous(
      labels = scales::label_number(
        big.mark = ".",     # separador de miles
        decimal.mark = ","  # separador decimal
      ))
  
  
  # --- 7. Versión interactiva con plotly ---
  graf_plotly <- plotly::ggplotly(graf+
                                    theme(
                                    #  axis.text.x = element_blank(),
                                      #axis.ticks = element_blank(),
                                      legend.position = "none",
                                      plot.title = element_text(face = "bold", hjust = 0.5)
                                    ), tooltip = "text") %>%
    plotly::layout(
      hoverlabel = list(bgcolor = "white", font = list(size = 12)))
S=BD[BD$var1==max(as.numeric(as.character(BD$var1))),][1,]    
Texto=paste("En el año ",max(as.numeric(as.character(BD$var1)))," se puede observar que en el 25% de los meses se alcanza un volumen de alimentos menor o igual a ",format(S$q1,decimal.mark=",",big.mark="."),
            "En el 50% de los meses se alcanza un volumen menor o igual a ",format(S$mediana,decimal.mark=",",big.mark="."),
            "En el 75% de los meses alcanza un volumen menor o igual a  ",format(S$q3,decimal.mark=",",big.mark="."))
  # --- 8. Retorno ---
  return(list(
    grafico_plano = graf,
    grafico_plotly = graf_plotly,
    datos = BD,
    Text_=Texto,
    resumen = resumen
  ))
}


Caja_y_Bigotes(ALIMENTO ="Repollo",formato="numeric")
Caja_y_Bigotes(ALIMENTO =NULL,formato="numeric")
