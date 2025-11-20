#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 25/02/2024
#Fecha de ultima modificacion: 02/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);require(ggQC);require(plotly);library(arrow)
options(scipen = 999)
################################################################################-

acum_total <- readRDS("acum_total_1_8.RDS")
acum_total_lugar <- readRDS("acum_total_lugar_1_8.RDS")
acum_total_anio <- readRDS("acum_total_anio_1_8.RDS")
acum_total_anio_lugar <- readRDS("acum_total_anio_lugar_1_8.RDS")
acum_total_anio_mes <- readRDS("acum_total_anio_mes_1_8.RDS")
acum_total_anio_mes_lugar <- readRDS("acum_total_anio_mes_lugar_1_8.RDS")

lugar_entra<-unique(acum_total_lugar$lugar[acum_total_lugar$origen=="Neto_entra"])
lugar_sale<-unique(acum_total_lugar$lugar[acum_total_lugar$origen=="Neto_sale"])
lugar_entra_exter<-unique(acum_total_lugar$lugar[acum_total_lugar$origen=="Neto_entra_exter"])

pareto_graf<-function(pareto,año=NULL, Mes=NULL, sitio=NULL){
  if(is.null(año)&&is.null(Mes)&&is.null(sitio)){
    df<-acum_total
    df<-df%>%
      filter(origen==pareto)%>%
      mutate(total_sum=total_sum/1000,
             graficar=ifelse(acumulado_total<=0.85,1,0))
  }else if(!is.null(año)&&is.null(Mes)&&is.null(sitio)){
    df<-acum_total_anio
    df<-df%>%
      filter(origen==pareto)%>%
      filter(anio==año)%>%
      mutate(total_sum=total_sum/1000,
             graficar=ifelse(acumulado_total<=0.85,1,0))
  }else if(!is.null(año)&&!is.null(Mes)&&is.null(sitio)){
    df<-acum_total_anio_mes
    df<-df%>%
      filter(origen==pareto)%>%
      filter(anio==año)%>%
      filter(mes==Mes)%>%
      mutate(total_sum=total_sum/1000,
             graficar=ifelse(acumulado_total<=0.85,1,0))
  }else if(is.null(año)&&is.null(Mes)&&!is.null(sitio)){
    df<-acum_total_lugar
    df<-df%>%
      filter(origen==pareto)%>%
      filter(lugar==sitio)%>%
      mutate(total_sum=total_sum/1000,
             graficar=ifelse(acumulado_total<=0.85,1,0))
  }else if(!is.null(año)&&is.null(Mes)&&!is.null(sitio)){
    df<-acum_total_anio_lugar
    df<-df%>%
      filter(origen==pareto)%>%
      filter(anio==año)%>%
      filter(lugar==sitio)%>%
      mutate(total_sum=total_sum/1000,
             graficar=ifelse(acumulado_total<=0.85,1,0))
  }else if(!is.null(año)&&!is.null(Mes)&&!is.null(sitio)){
    df<-acum_total_anio_mes_lugar
    df<-df%>%
      filter(origen==pareto)%>%
      filter(anio==año)%>%
      filter(mes==Mes)%>%
      filter(lugar==sitio)%>%
      mutate(total_sum=total_sum/1000,
             graficar=ifelse(acumulado_total<=0.85,1,0))
  }
  
  total_sum_total <- sum(df$total_sum)
  
  if (nrow(df) > 0 && any(df$graficar == 1)) {
    df_filtrado <- df %>% filter(graficar == 1)
  } else {
    df_filtrado <- df
  }
  
  num_productos <- nrow(df_filtrado)
  
  df_filtrado$tooltip_text1 <- paste0("Producto: ", df_filtrado$producto, "<br>Cantidad: ", format(round(df_filtrado$total_sum,1),decimal.mark=",",big.mark="."))
  df_filtrado$tooltip_text2 <- paste0("Producto: ", df_filtrado$producto, "<br>Porcentaje acumulado: ", round(df_filtrado$acumulado_total*100,1),"%")
  
  index <- which.min(abs(df_filtrado$acumulado_total - 0.8))
  # Si el valor es menor que 0.8, tomar el siguiente valor
  if(nrow(df_filtrado)!=0){if (df_filtrado$acumulado_total[index] < 0.8) {
    index <- index + 1
  }}else{
    ("")
  }
  
  # Obtener el valor más cercano a 0.8 (hacia arriba)
  acumulado <- (df_filtrado$acumulado_total[index])
  posicion_80 <- min(df_filtrado$producto[df_filtrado$acumulado_total == acumulado])  # Encuentra el valor mínimo donde se alcanza el 80%
 if(nrow(df) > 0){
  plot <- ggplot(df_filtrado, aes(x = reorder(producto, -total_sum), y = total_sum)) +
    geom_bar(stat = "identity", fill = "#2A4E61", aes(text = tooltip_text1)) +
    geom_line(aes(y = acumulado_total * total_sum_total), color = "#0087CF", group = 1) +
    geom_point(aes(y = acumulado_total * total_sum_total, text = tooltip_text2), color = "#0087CF", group = 1) +
    xlab("") + 
    geom_vline(xintercept = posicion_80, color = "#1877AA", linetype = "dashed") +
    ylab("Toneladas") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(
      labels = scales::label_number(
        big.mark = ".",     # separador de miles
        decimal.mark = ","  # separador decimal
      ))+
    labs(fill = "Barras: Amarillo", color = "Verde: Acumulado")
  p<-plotly::ggplotly(plot, tooltip = "text")

    

  }else{
    p<-print("No hay datos disponibles")
  }
  
  
  # Encontrar el índice del valor más cercano a 0.8 (hacia arriba)
  index <- which.min(abs(df_filtrado$acumulado_total - 0.8))
  
  # Si el valor es menor que 0.8, tomar el siguiente valor
  if(nrow(df_filtrado)!=0){if (df_filtrado$acumulado_total[index] < 0.8) {
    index <- index + 1
  }}else{
    ("")
  }
  
  # Obtener el valor más cercano a 0.8 (hacia arriba)
  acumulado <- round(df_filtrado$acumulado_total[index] * 100, digits = 1)
  df<-df%>%
    select(-graficar,-origen)
  prod_neces<-nrow(df_filtrado[df_filtrado$acumulado_total <= acumulado/100+0.001, ])
  porcent_prod<-round((prod_neces/length(df$producto))*100, digits = 1)
  plot<-plot+
    geom_text(aes(y = acumulado_total * total_sum_total, label = scales::percent(round(acumulado_total,3))), hjust = 1, color = "#000000", angle=90, size = 3)
  
  return(list(
    grafico_plano=plot,
    grafico=p,
    datos=df,
    porcent_prod=porcent_prod,
    acumulado=acumulado,
    prod_neces=prod_neces,
    num_productos = num_productos
  ))
  
}

#algo<-"Neto_entra"
#pareto_graf(pareto = algo,Mes = 4,año = 2023, sitio = "Bogotá")$grafico_plano
#pareto_graf(pareto = algo,año = 2013, sitio = "Antioquia")
#pareto_graf(pareto = algo, sitio = "Villavicencio")
#pareto_graf(pareto = algo,Mes = 2,año = 2024)
#pareto_graf(pareto = algo,año = 2013)$grafico_plano
#pareto_graf(pareto = algo)
