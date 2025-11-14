#Proyecto FAO
#INDICE Herfindahl–Hirschman - Abastecimiento tablero 1 
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(extrafont);library(plotly);library(arrow)
options(scipen = 999)
################################################################################
rm(list = ls())
#font_import(pattern = "Prompt.ttf", prompt = FALSE)
#loadfonts(device = "win")
################################################################################
Sys.setlocale("LC_TIME", "Spanish")

IHH_anual<-readRDS("IHH_anual_abastecimiento_2_3.RDS")
IHH_total<-readRDS("IHH_total_abastecimiento_2_3.RDS")
IHH_mensual<-readRDS("IHH_mensual_abastecimiento_2_3.RDS")%>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))


mapeo_meses <- c("ene" = "enero", "feb" = "febrero", "mar" = "marzo", "abr" = "abril", 
                 "may" = "mayo", "jun" = "junio", "jul" = "julio", "ago" = "agosto", 
                 "sep" = "septiembre", "oct" = "octubre", "nov" = "noviembre", "dic" = "diciembre")


# Función para producir un gráfico de tiempo
plot_data <- function(tipo, anio = NULL) {
  # Elegir el data frame correcto y el título
  if (tipo == 1) {
    data <- IHH_anual
    data <- rename(data, date_col = year)
    data$year <- data$date_col
    data$IHH <- data$IHH * 100 
    
    # Crear un gráfico de tiempo
    data$tooltip_text <- paste("Año: ", data$year , "<br> IHH:" , round(data$IHH,1))
    p_plano <- ggplot(data, aes_string(x = "date_col", y = "IHH")) +
      geom_line(color = "#983136") +
      geom_point(aes(text = tooltip_text),size = 1e-8) +
      labs(x = "Fecha", y = " ") +
      theme_minimal() +  
      scale_color_manual(values = "#983136") + 
      theme(text = element_text( size = 16)) + 
      scale_x_continuous(
        breaks = data$year
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
  } else {
    data <- IHH_mensual
    data <- rename(data, date_col = mes_y_ano)
    data$IHH <- data$IHH *100
    data$month_completo <- mapeo_meses[data$month]
    data$tooltip_text <- paste("Año: ", data$year ,"<br> Mes:" , data$month_completo, "<br> IHH:" , round(data$IHH,1))
    
    
    # Si se especificó un año, filtrar los datos para ese año
    if (!is.null(anio)) {
    data$tooltip_text <- paste("Año: ", data$year , "<br> Mes:" , data$month_completo,  "<br> IHH:" , round(data$IHH,1))
      data <- data %>% filter(year == anio)
    }
    # Crear un gráfico de tiempo
    p_plano <- ggplot(data, aes_string(x = "date_col", y = "IHH")) +
      geom_line(color = "#983136") +
      geom_point(aes(text = tooltip_text),size = 1e-8) +
      labs(x = "Fecha", y = " ") +
      theme_minimal() +  # Usar un tema minimalista
      scale_color_manual(values = "#983136") +  # Establecer el color de la línea
      theme(text = element_text( size = 12))+
      scale_x_date(date_breaks = "4 month", date_labels = "%Y-%m") +  # Establecer el formato de fecha
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    # Establecer la fuente y el tamaño del texto
      
    if (!is.null(anio)) {
      p_plano<-p_plano+scale_x_date(date_breaks = "1 months", date_labels = "%b")#+  # Configurar el eje X
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        }
    }
  

  data<-data%>%select(-tooltip_text)
  # Calcular el valor máximo del índice de vulnerabilidad
  max_IHH <- which.max(data$IHH)
  max_IHH_value <- round(data$IHH[max_IHH], 3)
  mes_max_IHH <- data$month[max_IHH]
  anio_max_IHH <- data$year[max_IHH]
  
  p <- plotly::ggplotly(p_plano, tooltip = "text")
  
  return(list("plot" = p, "data" = data, "max_IHH" = max_IHH_value, "mes_max_IHH" = mes_max_IHH,"anio_max_IHH" = anio_max_IHH, grafico_plano = p_plano))
}



# Uso de la función
# Informacion Anual
#plot_data(1)
# Informmacion mensual - total
#plot_data(0)$plot
# Informacion mensual - por año
#plot_data(0,2013)

