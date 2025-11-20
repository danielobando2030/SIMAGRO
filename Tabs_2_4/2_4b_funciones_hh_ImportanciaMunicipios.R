#Proyecto FAO
#INDICE DE IMPORTANCIA DE LOS MUNICIPIOS EN EL ABASTECIMIENTO DE ANTIOQUIA
#FUNCIONES
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 24/04/2024
#Fecha de ultima modificacion: 24/04/2024
################################################################################
# Paquetes 
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(haven); library(DT);library(extrafont);library(plotly);library(arrow)
options(scipen = 999)
################################################################################
rm(list = ls())
#font_import(pattern = "Prompt.ttf")
#loadfonts(device = "win")
############
#source("007a_HHINDEX_participacion_municipios.R")
# Bases de datos 
IHH_anual_producto <- readRDS("base_IHH_anual_producto_importanciadelosmunicipios_2_4.RDS") %>%
  rename(year = anio)
IHH_anual_total <- readRDS("base_IHH_anual_total_importanciadelosmunicipios_2_4.RDS") %>%
  rename(year = anio)
IHH_mensual_producto <- readRDS("base_IHH_mensual_producto_importanciadelosmunicipios_2_4.RDS") %>%
  mutate(mes_y_ano = as.Date(mes_y_ano, format = "%Y-%m-%d")) 
IHH_mensual_total <- readRDS("base_IHH_mensual_total_importanciadelosmunicipios_2_4.RDS") %>%
  mutate(mes_y_ano = as.Date(mes_y_ano, format = "%Y-%m-%d"))

nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")


# FUNCIONES  
# Temporal (serie de tiempo)

col_palette <-  c(
  "#983136", "#8A2C30", "#7C272B", "#6F2226", "#611D21", "#53181C",
  "#451317", "#370E12", "#2A090D",
  "#B34348", "#C0595C", "#CD6F71",
  "#DA8586", "#E79B9B", "#F4B1B0",
  "#F8C6C5", "#FBDAD9", "#FDEDED")


grafica_indice_mun <- function(tipo, anio_seleccionado = "", productos_seleccionados = "") {
  if (tipo == 1 ) {
    df <- IHH_anual_total
    df$IHH <- df$IHH * 100
    df <- df %>%
      select("year", "IHH")
  } else if (tipo == 2) {
    df <- IHH_anual_producto
    df$IHH <- df$IHH * 100
    df <- df %>%
      select("year","producto", "IHH")
    if (length(productos_seleccionados) == 0){
      stop("Para esta opcion debe escoger los productos que quiere graficar")
    }
  } else if (tipo == 3) {
    df <- IHH_mensual_total
    df$IHH <- df$IHH * 100
    df <- df %>%
      select("mes_y_ano","year","month","IHH")
    if (anio_seleccionado != ""){
      df <- df %>%
        filter(anio_seleccionado == year)
    }
  } else if (tipo == 4) {
    df <- IHH_mensual_producto
    df$IHH <- df$IHH * 100
    df <- df %>%
      select("year","month","mes_y_ano","producto", "IHH")
    if (anio_seleccionado != ""){
      df <- df %>%
        filter(anio_seleccionado == year)
    }
  }
  if (tipo == 2) {
    df <- rename(df, fecha = year)
    df$tooltip_text <- paste("Año: ", df$fecha , "<br> Producto:",df$producto, "<br> IHH:" , round(df$IHH,1))
    df <- df[df$producto %in% productos_seleccionados, ]
    p_plano <- ggplot(df, aes(x = fecha, y = IHH, color = producto)) +
      geom_line() +
      geom_point(aes(text = tooltip_text), size = 1e-8) +
      labs(x = "Fecha", y = "Índice diversidad de origen") +
      theme_minimal() +
      scale_color_manual(values = col_palette) +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 8)) +
      scale_x_continuous(breaks = seq(min(df$fecha), max(df$fecha), by = 1))
}else if (tipo == 4){
   df$mes_nombre <- nombres_meses[df$month]
    df <- rename(df, fecha = mes_y_ano)
    df <- df[df$producto %in% productos_seleccionados, ]
    df$tooltip_text <- paste("Año: ", df$year ,"<br> Mes:",df$mes_nombre, "<br> Producto:",df$producto, "<br> IHH:" , round(df$IHH,1))
    p_plano <- ggplot(df, aes(x = fecha, y = IHH, color = producto)) +
      geom_line() +
      geom_point(aes(text = tooltip_text),size = 1e-8) +
      labs(x = "Fecha", y = "Índice diversidad de origen") +
      theme_minimal() +
      scale_color_manual(values = col_palette) + 
      theme(text = element_text(size = 16),
            axis.text.x = element_text(size = 8, angle = 90, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months")
    
    if ( anio_seleccionado != "") {
      p_plano<-p_plano+scale_x_date(date_breaks = "1 months", date_labels = "%b")#+  # Configurar el eje X
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
    
    
    
  } else if (tipo == 1)  {
       df <- rename(df, fecha = year) 
       df$tooltip_text <- paste("Año: ", df$fecha , "<br> IHH:" , round(df$IHH,1))
       p_plano <- ggplot(df, aes(x = fecha, y = IHH)) +
         geom_line(color = "#983136") +
         geom_point(aes(text = tooltip_text),size = 1e-8) +
         labs(x = "Fecha", y = "Índice diversidad de origen") +
         theme_minimal() +
         theme(text = element_text( size = 16),
               axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
         scale_x_continuous(breaks = unique(df$fecha))+
         scale_x_continuous(breaks = seq(min(df$fecha), max(df$fecha), by = 1))
       
    }else if (tipo == 3){
      df$mes_nombre <- nombres_meses[df$month]
      df <- rename(df, fecha = mes_y_ano)
      df$tooltip_text <- paste("Año:", df$year ,"<br> Mes:",df$mes_nombre, "<br> IHH:" , round(df$IHH,1))
      p_plano <- ggplot(df, aes(x = fecha, y = IHH)) +
        geom_line(color = "#983136") +
        geom_point(aes(text = tooltip_text),size = 1e-8) +
        labs(x = "Fecha", y = "Índice diversidad de origen") +
        theme_minimal()  +
        scale_color_manual(values = col_palette) +
        theme(text = element_text(size = 12),
              axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
        scale_x_date(date_breaks = "4 month", date_labels = "%Y-%m")
      
      if ( anio_seleccionado != "") {
        p_plano<-p_plano+scale_x_date(date_breaks = "1 months", date_labels = "%b")#+  # Configurar el eje X
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }
      
    }
    
  
  p <- plotly::ggplotly(p_plano, tooltip = "text")
  
  # Calcular el valor máximo del índice de vulnerabilidad
  indice_max_ihh <- which.max(df$IHH)
  max_IHH <- round(df$IHH[indice_max_ihh], 1)
  fecha_max_vulnerabilidad <- df$fecha[indice_max_ihh]
  producto_max_vulnerabilidad <- ifelse("producto" %in% names(df), df$producto[indice_max_ihh], NA)
  df<-df%>%select(-tooltip_text)
  # Devolver el gráfico, los datos y los valores máximos
  return(list(
    grafico = p,
    grafico_plano = p_plano,
    datos = df,
    max_vulnerabilidad = max_IHH,
    fecha_max_vulnerabilidad = fecha_max_vulnerabilidad,
    producto_max_vulnerabilidad = producto_max_vulnerabilidad
  ))
} 


# Ejemplos:
# Informacion anual 
#grafica_indice_mun(1)
#grafica_indice_mun(2,"",c("ARROZ","CARNE DE CERDO","FRÍJOL"))
#grafica_indice_mun(3)
#grafica_indice_mun(3,2023)
#grafica_indice_mun(4,"",c("ARROZ","CARNE DE CERDO","FRÍJOL"))
#grafica_indice_mun(4,2022,c("ARROZ","CARNE DE CERDO","FRÍJOL"))







