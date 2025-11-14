#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 01/04/2024
#Fecha de ultima modificacion: 02/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(plotly);library(arrow)
options(scipen = 999)
################################################################################-

proviene_cundinamarca<-readRDS("base_indices_sale_cundinamarca_1_4.RDS")

#####

tiempo <- function(opcion1, opcion2 = NULL, opcion3 = NULL, opcion4 = NULL) {
  df <- proviene_cundinamarca
  
  # Si opcion4 no es NULL, filtrar por año
  if (!is.null(opcion4)) {
    df <- df %>% filter(anio == opcion4)
  }
  
  if (opcion1 == "total") {
    df <- df %>%
      distinct(anio, mes, .keep_all = TRUE) %>%
      select(anio, mes, mes_y_ano, total_toneladas_mes)
    
    ggplot(df, aes(x = mes_y_ano, y = total_toneladas_mes)) +
      geom_line() +
      labs(title = "Total de toneladas que salen de Cundinamarca por mes") +
      ylab("Total de toneladas que saca de Cundinamarca por mes") +
      xlab("Información por meses") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  
    
  } else if (opcion1 == "mpio_destino") {
    if (is.null(opcion2)) {
      stop("Debe proporcionar un municipio cuando opcion1 es 'mpio_destino'")
    }
    
    df <- df %>%
      distinct(anio, mes, mpio_destino, .keep_all = TRUE) %>%
      select(anio, mes, mes_y_ano, total_toneladas_anio_mes_municipio, mpio_destino) %>%
      filter(mpio_destino == opcion2)
    
    ggplot(df, aes(x = mes_y_ano, y = total_toneladas_anio_mes_municipio)) +
      geom_line() +
      labs(title = "Total de toneladas por municipio por mes") +
      ylab("Total de toneladas que saca de Cundinamarca por mes") +
      xlab("Información por meses") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  } else if (opcion1 == "producto") {
    df <- df %>%
      distinct(anio, mes, producto, .keep_all = TRUE) %>%
      select(anio, mes, mes_y_ano, total_toneladas_mes_producto, producto)
    
    if (!is.null(opcion3)) {
      df <- df %>%
        filter(producto == opcion3)
    }
    
    ggplot(df, aes(x = mes_y_ano, y = total_toneladas_mes_producto, color = producto)) +
      geom_line() +
      labs(title = paste("Total de toneladas de", opcion3, "por mes")) +
      ylab("Total de toneladas") +
      xlab("Información Mensual") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Elimina la cuadrícula
    
  } else if (opcion1 == "mpio_destino_producto") {
    if (is.null(opcion2) || is.null(opcion3)) {
      stop("Debe proporcionar un municipio y un producto cuando opcion1 es 'mpio_destino_producto'")
    }
    
    df <- df %>%
      distinct(anio, mes, mpio_destino, producto, mes_y_ano, .keep_all = TRUE) %>%
      select(anio, mes, mes_y_ano, total_toneladas_anio_mes_municipio_producto, mpio_destino, producto) %>%
      filter(mpio_destino == opcion2, producto == opcion3)
    
    ggplot(df, aes(x = mes_y_ano, y = total_toneladas_anio_mes_municipio_producto, color = producto)) +
      geom_line() +
      labs(title = paste("Total de toneladas de" , opcion3 , "que van al municipio de ", opcion2)) +
      ylab("Total toneladas") +
      xlab("Información Mensual") +
      theme_classic()  
  }
}

# Para obtener el Total de toneladas que saca de Antioquia por mes
#tiempo("total")
# Para obtener el Total de toneladas que saca de Antioquia por mes de un municipio específico
#tiempo("mpio_destino", "Medellín")
# Para obtener el total de toneladas de un producto específico que ingresan a Medellín por mes
#tiempo("producto", "", "arroz")
# Para obtener el total de toneladas de un producto específico que ingresan a Medellín por mes de un municipio específico
#tiempo("mpio_destino_producto", "Medellín", "arroz")
# Para obtener el Total de toneladas que saca de Antioquia por mes en un año específico
#tiempo("total", "" , "", 2021)
# Para obtener el Total de toneladas que saca de Antioquia por mes de un municipio específico en un año específico
#tiempo("mpio_destino", "Medellín", "", 2021)
# Para obtener el total de toneladas de un producto específico que ingresan a Medellín por mes en un año específico
#tiempo("producto", "", "arroz", 2018)
# Para obtener el total de toneladas de un producto específico que ingresan a Medellín por mes de un municipio específico en un año específico
#tiempo("mpio_destino_producto", "Medellín", "arroz", 2021)



# Funcion Numero 2

importancia <- function(Año = NULL, Mes = NULL, municipios = 10, Producto = NULL) {
  df <- proviene_cundinamarca
  
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
      distinct(anio,mpio_destino,producto,mes, .keep_all = TRUE) %>%
      select(anio,mpio_destino, mes, producto, mes_municipio_producto_porcentaje)  
    df <- df %>% rename( columna_porcentaje = mes_municipio_producto_porcentaje)
    
  } else if (!is.null(Año) && !is.null(Mes)) {
    
    df<- df  %>%
      distinct(anio,mpio_destino,mes, .keep_all = TRUE) %>%
      select(anio,mpio_destino, mes, mes_municipio_porcentaje)  
    columna_porcentaje <- "mes_municipio_porcentaje"
    df <- df %>% rename( columna_porcentaje = mes_municipio_porcentaje)
    
    
  } else if (!is.null(Año) && !is.null(Producto)) {
    
    df<- df  %>%
      distinct(anio,mpio_destino,producto, .keep_all = TRUE) %>%
      select(anio,mpio_destino, producto, anio_municipio_producto_porcentaje)  
    columna_porcentaje <- "anio_municipio_producto_porcentaje"
    df <- df %>% rename( columna_porcentaje =anio_municipio_producto_porcentaje)
    
  } else if (!is.null(Año)){
    # No se tienen ni mes ni producto
    
    df<- df  %>%
      distinct(anio,mpio_destino, .keep_all = TRUE) %>%
      select(anio,mpio_destino, anio_municipio_porcentaje)  
    df <- df %>% rename( columna_porcentaje = anio_municipio_porcentaje)
    
  }else if (!is.null(Producto)){
    # No se tienen ni mes ni producto
    
    df<- df  %>%
      distinct(mpio_destino,producto, .keep_all = TRUE) %>%
      select(mpio_destino,producto, municipio_producto_porcentaje)  
    df <- df %>% rename( columna_porcentaje = municipio_producto_porcentaje)
    
  } else {
    
    df<- df  %>%
      distinct(mpio_destino, .keep_all = TRUE) %>%
      select(mpio_destino, municipio_porcentaje)  
    df <- df %>% rename( columna_porcentaje = municipio_porcentaje)
    
  }
  
  if(nrow(df)==0){
    p<-  validate("No hay datos disponibles")
    p_plano <- NULL
  } else {
  
  df <- df  %>% 
    arrange(desc(all_of(columna_porcentaje))) %>% 
    mutate(mpio_destino = factor(mpio_destino, levels = mpio_destino)) %>% 
    head(municipios)
  
  # Grafica  
  # Crear un título dinámico
  titulo <- paste0("Importancia de los ", length(unique(df$mpio_destino)), " municipios principales", ifelse(is.null(Año), "", paste0(" en el año ", Año)), 
                  ifelse(is.null(Mes), "", paste0(" en el mes de ", Mes)), 
                  ifelse(is.null(Producto), "", paste0(" - ", Producto)))
  
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

  
    df$tooltip_text <- paste("Ciudad destino: ", df$mpio_destino, "<br>Porcentaje: ", round(df$columna_porcentaje*100,digits = 1),"%")
    
    # Ahora puedes usar col_palette en tu gráfico
    p_plano<-ggplot(df, aes(x = forcats::fct_reorder(mpio_destino, as.numeric(all_of(columna_porcentaje))), y = as.numeric(all_of(columna_porcentaje)), fill = mpio_destino,text = tooltip_text)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(as.numeric(all_of(columna_porcentaje)), accuracy = 0.1)), hjust = 0.1) +
      coord_flip() +
      labs(x = "", y = "", title = "") +
      scale_fill_manual(values = col_palette) +  # Agregar la paleta de colores
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    
    p <- plotly::ggplotly(p_plano, tooltip = "text")
  }
  df<-df%>%select(-tooltip_text)
  porcentaje_max <- ifelse(nrow(df)==0, "", round(max(df$columna_porcentaje)*100, digits = 2))
  lugar_max <- ifelse(nrow(df) == 0, "", as.character(df$mpio_destino)[which.max(df$columna_porcentaje)])
  
  return(
    list(
      grafico = p,
      grafico_plano = p_plano,
      datos = df,
      porcentaje_max=porcentaje_max,
      lugar_max=lugar_max
    )
  )

  }

#importancia(2023)
#importancia(2023,1)
#importancia(2023,1,15,"Lechuga batavia")
#importancia(Año = 2018, Producto = "Lechuga batavia")
#importancia(municipios = 8,Producto = "Carne de cerdo")
#importancia()

