#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 24/02/2024
#Fecha de ultima modificacion: 24/02/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(arrow)
library(dplyr);library(ggplot2);library(scales)
options(scipen = 999)
################################################################################-


abastecimiento_bogota_ANO_mes_alimento=readRDS("abastecimiento_bogota_ANO_mes_alimento_2_1.RDS")
abastecimiento_bogota_ANO_alimento=readRDS("abastecimiento_bogota_ANO_alimento_2_1.RDS")
abastecimiento_bogota_alimento=readRDS("abastecimiento_bogota_alimento_2_1.RDS")
abastecimiento_bogota_total=readRDS("abastecimiento_bogota_total_2_1.RDS")
abastecimiento_bogota_ANO=readRDS("abastecimiento_bogota_ANO_2_1.RDS")
abastecimiento_bogota_ANO_mes=readRDS("abastecimiento_bogota_ANO_mes_2_1.RDS")



SLIDER <- round(seq(0.0303, 1, by=0.0303), 2)

Lorentz_GINI = function(ANO=NULL, ALIMENTO=NULL, MES=NULL){
  if (is.null(ANO) && is.null(ALIMENTO) && is.null(MES)) {
    df=abastecimiento_bogota_total
  }
  
  # 2. Filtro solo por alimento
  if (is.null(ANO) && !is.null(ALIMENTO) && is.null(MES)) {
    df=subset(abastecimiento_bogota_alimento, producto == ALIMENTO)
  }
  
  # 3. Filtro por año y alimento
  if (!is.null(ANO) && !is.null(ALIMENTO) && is.null(MES)) {
    df=subset(abastecimiento_bogota_ANO_alimento,
                  anio == ANO & producto == ALIMENTO)
  }
  
  # 4. Filtro por año, alimento y mes
  if (!is.null(ANO) && !is.null(ALIMENTO) && !is.null(MES)) {
    df=subset(abastecimiento_bogota_ANO_mes_alimento,
                  anio == ANO & producto == ALIMENTO & mes == MES)
  }
  
  # 5. Filtro solo por año
  if (!is.null(ANO) && is.null(ALIMENTO) && is.null(MES)) {
    df=subset(abastecimiento_bogota_ANO, anio == ANO)
  }
  
  # 6. Filtro por año y mes (sin alimento)
  if (!is.null(ANO) && is.null(ALIMENTO) && !is.null(MES)) {
    df=subset(abastecimiento_bogota_ANO_mes,
                  anio == ANO & mes == MES)
  }
  
  # 7. Filtro solo por mes (sin año ni alimento)
  if (is.null(ANO) && is.null(ALIMENTO) && !is.null(MES)) {
    df=subset(abastecimiento_bogota_mes, mes == MES)
  }
  
  # Si ningún caso coincide (por seguridad)
  # Calcular proporciones
  Salida = df
  Salida$Part=Salida$suma_kg/sum(Salida$suma_kg)
  Salida=Salida%>%arrange(Part)
  Salida$Part_cum = cumsum(Salida$Part)
  Salida$one_cum = cumsum(rep(1, nrow(Salida))/nrow(Salida))
  fila_inicial <- Salida[1, ]  # tomar estructura para conservar columnas
  fila_inicial[,] <- NA        # limpiar valores
  fila_inicial$Part <- 0
  fila_inicial$Part_cum <- 0
  fila_inicial$one_cum <- 0
  Salida <- bind_rows(fila_inicial, Salida)
  
  # Coordenadas del punto con el slider
  #coord_x = Salida$one_cum[which.min(abs(slider - Salida$one_cum))]
  #coord_y = Salida$Part_cum[which.min(abs(slider - Salida$one_cum))]
  
  # Cálculo del Gini
  gini = 1 - 2 * sum((Salida$Part_cum + c(0, head(Salida$Part_cum, -1))) * (1/nrow(Salida))) / 2
  
  # Gráfico

  Salida$tooltip_text <- paste0("<br> % Municipios acumulados: ", scales::percent(Salida$one_cum,accuracy=0.1), "<br> % Kilogramos acumulados:", scales::percent(Salida$Part_cum,accuracy=0.1))
  

  
    graf <- ggplot(Salida, aes(x=one_cum, y=Part_cum)) +
      geom_line(aes(group = 1, text = tooltip_text), color =  "#983136", size = 1.2)+ # curva de Lorenz
    geom_abline(intercept=0, slope=1, linetype="dashed", color="#743639") + # línea de igualdad
      #geom_segment(aes(x=coord_x, xend=coord_x, y=0, yend=coord_y), 
      #           linetype="dotted", color="gray40") +
    #geom_segment(aes(x=0, xend=coord_x, y=coord_y, yend=coord_y), 
    #             linetype="dotted", color="gray40") +
    # punto del slider
  #  geom_point(aes(x=coord_x, y=coord_y), color="darkorange", size=4) +
    # etiqueta del punto
    #geom_text(aes(x=coord_x, y=coord_y, 
    #              label=paste0("(", percent(coord_x,0.1), ", ", percent(coord_y,0.1), ")")),
    #          vjust=-1, hjust=0.5, size=3.5, color="black") +
    
    # texto Gini
    annotate("text", x=0.1, y=0.85, 
             label=paste0("Gini = ", round(gini, 3)), 
             hjust=0, size=5, color="#4F3032", fontface="bold") +
    # etiquetas y escalas
    labs(x="Proporción acumulada de Municipios de origen",
         y="Proporción acumulada kilogramos") +
    scale_x_continuous(labels = scales::percent, limits=c(0,1)) +
    scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
    theme_minimal(base_size=14)
  p <- plotly::ggplotly(graf, tooltip = "text")
  ali=ifelse(is.null(ALIMENTO),"todos los alimentos",ALIMENTO)
  return(
    list(
      grafico_plano = graf,
      grafico_plotly = p,
      datos = Salida,
      gini_=gini
    ))
    
}

#Lorentz_GINI(ANO=NULL, ALIMENTO=NULL, MES=NULL, slider=0.7)

#ALIMENTO="Zanahoria" 
#ANO=2023
#MES=1
#Lorentz_GINI(ANO=2023, ALIMENTO=NULL, MES=NULL)$grafico_plano

#Lorentz_GINI(ANO=2023, ALIMENTO="Zanahoria", MES=1)$grafico_plano
#Lorentz_GINI(ANO=2023, ALIMENTO="Zanahoria", MES=NULL)$grafico_plano
#Lorentz_GINI(ANO=2023, ALIMENTO=NULL, MES=1)$grafico_plano




