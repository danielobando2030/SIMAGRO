  #Proyecto FAO
  #INDICE Herfindahl–Hirschman - Exportaciones 1 - Hacia donde va la comida que se produce en Antioquia
  # Funciones
  ################################################################################
  #Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
  #Fecha de creacion: 29/04/2024
  #Fecha de ultima modificacion:29/04/2024
  ################################################################################
  # Paquetes 
  library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
  library(glue);library(tidyverse);library(DT);library(tools)
  library(plotly)
  options(scipen = 999)
  ################################################################################
  rm(list = ls())

  # Cargamos las bases de datos generadas en 008a_HHINDEX_participacion_destino
  IHH_anual_total <- readRDS("base_indice_anual_total_destino_2_5.RDS")
  IHH_anual_total$year <-as.numeric(IHH_anual_total$year)
  IHH_anual_producto <- readRDS("base_indice_anual_producto_desti_2_5.RDS")%>%
    mutate(producto=tools::toTitleCase(tolower(producto)))
  IHH_anual_producto$year <- as.numeric(IHH_anual_producto$year)
  IHH_mensual_producto <- readRDS("base_indice_mensual_producto_destino_2_5.RDS")%>%
    mutate(producto=tools::toTitleCase(tolower(producto)))
  IHH_mensual_total <- readRDS("base_indice_mensual_total_destino_2_5.RDS")
  
  nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
# Funcion 

  col_palette <-  c(
    "#983136", "#8A2C30", "#7C272B", "#6F2226", "#611D21", "#53181C",
    "#451317", "#370E12", "#2A090D",
    "#B34348", "#C0595C", "#CD6F71",
    "#DA8586", "#E79B9B", "#F4B1B0",
    "#F8C6C5", "#FBDAD9", "#FDEDED")
grafica_indice <- function(tipo, anio_seleccionado = "", productos_seleccionados = "") {
  if (tipo == 1 ) {
    df <- IHH_anual_total
    df <- df %>%
      select("year", "IHH")
  } else if (tipo == 2) {
    df <- IHH_anual_producto
    df <- df %>%
      select("year","producto", "IHH")
    
  } else if (tipo == 3) {
    df <- IHH_mensual_total
    df <- df %>%
      select("mes_y_ano","year","month","IHH")
    if (anio_seleccionado != ""){
      df <- df %>%
        filter(anio_seleccionado == year)
    }
  } else if (tipo == 4) {
    df <- IHH_mensual_producto
    df <- df %>%
      select("year","month","mes_y_ano","producto", "IHH")
    if (anio_seleccionado != ""){
      df <- df %>%
        filter(anio_seleccionado == year)
    }
  }
  if (tipo %in% c(1, 2)) {
    df <- rename(df, fecha = year) 
  }else if (tipo %in% c(3, 4)){
    df <- rename(df, fecha = mes_y_ano)
    df$fecha <-  as.Date(df$fecha)
  }
  df<-df%>%mutate(IHH=round(IHH*100))
  # Filtrar los productos seleccionados solo para las opciones 2 y 4
  if (tipo %in% c(1)) {
    # Comprueba si df$fecha está vacío o contiene valores no numéricos
    df$tooltip_text <- paste("Año: ", df$fecha , "<br> IHH:" , round(df$IHH,1))  
    p_plano<-ggplot(df, aes(x = fecha, y = IHH)) +
        geom_line(color = "#983136") +
        geom_point(aes(text = tooltip_text), size = 1e-8) +
        labs(x = "Fecha", y = " ") +
        scale_x_continuous(breaks = seq(min(df$fecha), max(df$fecha))) +
        scale_color_manual(values = col_palette) +
        theme_minimal()  +
        theme(text = element_text( size = 16),
              axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
        scale_x_continuous(breaks = unique(df$fecha))+
        scale_x_continuous(breaks = seq(min(df$fecha), max(df$fecha), by = 1))
      
  }else if (tipo %in% c(2)){
    df <- df[df$producto %in% productos_seleccionados, ]
    df$tooltip_text <- paste("Año: ", df$fecha , "<br> Producto:",df$producto, "<br> IHH:" , round(df$IHH,1))
    p_plano<-ggplot(df, aes(x = fecha, y = IHH, color = producto)) +
      geom_line() +
      geom_point(aes(text = tooltip_text), size = 1e-8) +
      scale_color_manual(values = col_palette) +
      labs(x = "Fecha", y = " ") +
      scale_x_continuous(breaks = seq(min(df$fecha), max(df$fecha))) +
      theme_minimal() +
      scale_color_manual(values = col_palette) +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 8)) +
      scale_x_continuous(breaks = seq(min(df$fecha), max(df$fecha), by = 1))
    
    
  } else if (tipo%in%(3)) { 
    df$mes_nombre <- nombres_meses[df$month]
    df$tooltip_text <- paste("Año:", df$year ,"<br> Mes:",df$mes_nombre, "<br> IHH:" , round(df$IHH,1))
   p_plano<- ggplot(df, aes(x = fecha, y = IHH)) +
      geom_line(color = "#983136") +
     geom_point(aes(text = tooltip_text), size = 1e-8) +
      labs(x = "Fecha", y = " ") +
      theme_minimal()  +
     scale_color_manual(values = col_palette) +
     theme(text = element_text(size = 12),
           axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
     scale_x_date(date_breaks = "4 month", date_labels = "%Y-%m")
   
   if ( anio_seleccionado != "") {
     p_plano<-p_plano+scale_x_date(date_breaks = "1 months", date_labels = "%b")#+  # Configurar el eje X
     #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
   }
   
  } else if (tipo%in%(4)){
    df$mes_nombre <- nombres_meses[df$month]
    df <- df[df$producto %in% productos_seleccionados, ]
    df$tooltip_text <- paste("Año: ", df$year ,"<br> Mes:",df$mes_nombre, "<br> Producto:",df$producto, "<br> IHH:" , round(df$IHH,1))
    p_plano<-ggplot(df, aes(x = fecha, y = IHH, color = producto)) +
      geom_line() +
      geom_point(aes(text = tooltip_text), size = 1e-8) +
      labs(x = "Año", y = " ") +
      theme_minimal()  +
      scale_color_manual(values = col_palette) + 
      theme(text = element_text(size = 16),
            axis.text.x = element_text(size = 8, angle = 90, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months")
    
    if ( anio_seleccionado != "") {
      p_plano<-p_plano+scale_x_date(date_breaks = "1 months", date_labels = "%b")#+  # Configurar el eje X
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
      
  }
  
  # Calcular el valor máximo del índice de vulnerabilidad
  indice_max_vulnerabilidad <- which.max(df$IHH)
  max_vulnerabilidad <- round(df$IHH[indice_max_vulnerabilidad], 3)
  fecha_max_vulnerabilidad <- df$fecha[indice_max_vulnerabilidad]
  producto_max_vulnerabilidad <- ifelse("producto" %in% names(df), df$producto[indice_max_vulnerabilidad], NA)
  
  p<-plotly::ggplotly(p_plano,tooltip = "text")
  
  # Devolver el gráfico, los datos y los valores máximos
  return(list(
    grafico = p,
    grafico_plano = p_plano,
    datos = df,
    max_vulnerabilidad = max_vulnerabilidad,
    fecha_max_vulnerabilidad = fecha_max_vulnerabilidad,
    producto_max_vulnerabilidad = producto_max_vulnerabilidad
  ))
} 


#grafica_indice(1)
#grafica_indice(2,"",c("Arroz","Carne De Cerdo"))
#grafica_indice(3,2013)
#grafica_indice(4,"",c("Arroz","Carne De Cerdo"))
#grafica_indice(4,"2015",c("Arroz","Carne De Cerdo"))








