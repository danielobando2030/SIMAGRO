# Proyecto FAO
# Visualizacion de DATOS   - abastecimeinto en bogota 
################################################################################-
#Autores: Cristian Daniel Obando Arbeláez, Luis Miguel Garcia
#Fecha de creacion: 20/03/2024
#Fecha de ultima modificacion: 10/11/2025
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(tools);library(plotly);library(arrow)
options(scipen = 999)
################################################################################-
abastecimiento_bogota<-readRDS("base_indices_abastecimiento_1_1.RDS")%>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))

abastecimiento_bogota_interno<-readRDS("base_indices_abastecimiento_interno_1_1.RDS")%>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))

abastecimiento_bogota_externo<-readRDS("base_indices_abastecimiento_externo_1_1.RDS")%>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))


# Funcion Numero 2

importancia <- function(tipo, Año = NULL, Mes = NULL, municipios = 10, Producto = NULL) {
  if(tipo==1){
    df <- abastecimiento_bogota
  }else if(tipo==2){
    df <- abastecimiento_bogota_interno
  }else {
    df <- abastecimiento_bogota_externo 
  }
  
  
  if (is.null(municipios) || length(municipios) == 0) {
    return(NULL)
  }
  
  
  # Año seleccionado
  if (!is.null(Año)) {
    df <- df %>% dplyr::filter(anio == Año)
  }
  
  # Mes seleccionado, si se proporciona
  if (!is.null(Mes)) {
    df <- df %>% dplyr::filter(mes == Mes)
  }
  
  # Si se especifica un producto
  if (!is.null(Producto)) {
    df <- df %>% dplyr::filter(producto == Producto)
  }
  
  # Determinar la columna de porcentaje
  if (!is.null(Año) && !is.null(Mes) && !is.null(Producto)) {
    
    df<- df  %>%
      distinct(anio, mpio_origen,producto,mes, .keep_all = TRUE) %>%
      select(anio, mpio_origen,municipio_r, mes, producto, mes_municipio_producto_porcentaje)  
    df <- df %>% rename( columna_porcentaje = mes_municipio_producto_porcentaje)
    
  } else if (!is.null(Año) && !is.null(Mes)) {
    
    df<- df  %>%
      distinct(anio, mpio_origen,mes, .keep_all = TRUE) %>%
      select(anio, mpio_origen, municipio_r,mes, mes_municipio_porcentaje)  
    columna_porcentaje <- "mes_municipio_porcentaje"
    df <- df %>% rename( columna_porcentaje = mes_municipio_porcentaje)
    
    
  } else if (!is.null(Año) && !is.null(Producto)) {
    
    df<- df  %>%
      distinct(anio, mpio_origen,producto, .keep_all = TRUE) %>%
      select(anio, mpio_origen,municipio_r, producto, año_municipio_producto_porcentaje)  
    columna_porcentaje <- "año_municipio_producto_porcentaje"
    df <- df %>% rename( columna_porcentaje =año_municipio_producto_porcentaje)
    
  } else if (!is.null(Año)){
    # No se tienen ni mes ni producto
    
    df<- df  %>%
      distinct(anio, mpio_origen, .keep_all = TRUE) %>%
      select(anio, mpio_origen, municipio_r,año_municipio_porcentaje)  
    df <- df %>% rename( columna_porcentaje = año_municipio_porcentaje)
    
  }else if (!is.null(Producto)){
    # No se tienen ni mes ni producto
    
    df<- df  %>%
      distinct( mpio_origen,producto, .keep_all = TRUE) %>%
      select( mpio_origen,producto,municipio_r, municipio_producto_porcentaje)  
    df <- df %>% rename( columna_porcentaje = municipio_producto_porcentaje)
    
  } else {
    
    df<- df  %>%
      distinct( mpio_origen, .keep_all = TRUE) %>%
      select( mpio_origen, municipio_r, municipio_porcentaje)  
    
    df <- df %>% rename( columna_porcentaje = municipio_porcentaje)

  }
  
  
  municipios <- min(municipios, 18)
  
  df <- df  %>% 
    arrange(desc(all_of(columna_porcentaje))) %>% 
    mutate( mpio_origen = factor( mpio_origen, levels =  mpio_origen)) %>% 
    head(municipios)
  
  # Grafica  
  # Crear un título dinámico
  #titulo <- paste("Importancia de los", length(unique(df$ mpio_origen)), "municipios principales", ifelse(is.null(Año), "", paste("en el año", Año)), 
  #                ifelse(is.null(Mes), "", paste("en el mes de", Mes)), 
  #                ifelse(is.null(Producto), "", paste("-", Producto)))
  
  # Definir los colores de inicio y fin
  #start_color <- "#6B0077"
  #end_color <- "#ACE1C2"
  
  # Crear una función de interpolación de colores
  #color_func <- colorRampPalette(c(start_color, end_color))
  
  # Generar una paleta de N colores
  #N <- municipios
  #col_palette <- color_func(N)
  col_palette <-c(
    "#0F2F52", "#134174", "#1B5A9B", "#2371C2", "#3A8DE1", "#63A4F3",
    "#8BB8F7", "#B3CCFA", "#D9E4FF", "#A8C2E0", "#6F94B8", "#466B90",
    "#2B4E73", "#1D3C5A", "#0E2940", "#5E3C74", "#8B4FA0", "#C07DD2"
  )
  df$tooltip_text <- paste0("Ciudad de origen: ", df$mpio_origen, "<br>Porcentaje: ", round(df$columna_porcentaje*100,digits = 1),"%")
  
    graf <- ggplot(df, aes(x =  forcats::fct_reorder(municipio_r, as.numeric(all_of(columna_porcentaje))), y = as.numeric(all_of(columna_porcentaje)), fill =  mpio_origen, text = tooltip_text)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(as.numeric(all_of(columna_porcentaje)), accuracy = 0.1)), hjust = 1.2) +
    coord_flip() +
    labs(x = " ", y = "Porcentaje", title = " ") +
    scale_fill_manual(values = col_palette) +  # Agregar la paleta de colores
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.ticks.x = element_blank(), axis.text.x = element_blank())
  
  
  
  p <- plotly::ggplotly(graf, tooltip = "text")
  
  
  
  porcentaje_max<-round(max(df$columna_porcentaje)*100,1)
  lugar_max<-df$mpio_origen[which.max(df$columna_porcentaje)]
  df<-df%>%select(-tooltip_text)
  return(
    list(
      grafico_plano = graf,
      grafico_plotly = p,
      datos = df,
      porcentaje_max=porcentaje_max,
      lugar_max=lugar_max
    )
  )
}


importancia(1,2023)
#importancia(2,2023,1)
#importancia(3,2023,1,15,"lechuga batavia")
#importancia(1,Año = 2023, Producto = "lechuga batavia")
#importancia(2,municipios = 8,Producto = "carne de cerdo")
#importancia(3)


