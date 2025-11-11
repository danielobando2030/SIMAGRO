# Proyecto FAO
# Visualizacion de DATOS 2  - abastecimeinto en Medellin 
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 20/03/2024
#Fecha de ultima modificacion: 23/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
# install.packages("Sweave")
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(shiny); library(shinydashboard)
library(htmlwidgets);library(webshot);library(magick);library(shinyscreenshot);library(webshot2); library(knitr);library(rmarkdown)


options(scipen = 999)
################################################################################-
# Definir la función de servidor
server <- function(input, output, session) {
  resultado<-reactive({
    if (input$anio == "todo" && input$mes != "todo") {
      # No se puede seleccionar mes sin año
      validate(need(FALSE, "Debe seleccionar un año antes de elegir un mes."))
      
    } else if (input$anio == "todo" && input$producto == "todo" && input$mes == "todo") {
      # Todo seleccionado como "todo"
      Lorentz_GINI(ANO = NULL, ALIMENTO = NULL, MES = NULL)
      
    } else if (input$producto != "todo" && input$anio == "todo" && input$mes == "todo") {
      # Solo producto
      Lorentz_GINI(ANO = NULL, ALIMENTO = input$producto, MES = NULL)
      
    } else if (input$producto == "todo" && input$mes == "todo") {
      # Solo año
      Lorentz_GINI(ANO = input$anio, ALIMENTO = NULL, MES = NULL)
      
    } else if (input$producto == "todo") {
      # Año + mes
      Lorentz_GINI(ANO = input$anio, ALIMENTO = NULL, MES = input$mes)
      
    } else if (input$mes == "todo") {
      # Año + producto
      Lorentz_GINI(ANO = input$anio, ALIMENTO = input$producto, MES = NULL)
      
    } else {
      # Año + producto + mes
      Lorentz_GINI(ANO = input$anio, ALIMENTO = input$producto, MES = input$mes)
    }
  })
  

# Se genera el grafico plotly 
  
  output$grafico <- plotly::renderPlotly({
    res <- resultado()
    if (is.character(res) || length(res) == 0 ) {
      return(NULL)  #  No hay gráfico para mostrar
    } else {
      res$grafico_plotly  # Devuelve el gráfico Plotly
    }
  })
  
# Se genera el grafico plano
  
  grafico_plano <- reactive({
    res <- resultado()
    if (is.character(res) || length(res) == 0 ) {
      return(NULL)  # No hay gráfico para guardar
    } else {
      res$grafico_plano  # Guarda solo el gráfico 'grafico_plano'
    }
  })  
  
  

  output$descargar <- downloadHandler(
    filename = function() {
      paste("grafica-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      tempFile <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(as_widget(resultado()$grafico), tempFile, selfcontained = FALSE)
      webshot::webshot(tempFile, file = file, delay = 2)
    }
  )
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  
  
# Generamos el subtitulo 
# Values se debe generar para guardar la variable que se lleva al rmd  
  values <- reactiveValues(subtitulo = NULL)  
  
  output$subtitulo <- renderText({
    res <- resultado()
    if(nrow(res$datos) == 0){
      values$subtitulo <- "No hay datos disponibles"
    }else{
    values$subtitulo= "Esta visualización muestra el porcentaje acumulado de municipios que envían sus productos a Cundinamarca. 
  Cuanto más cercana esté la curva a la línea de 45°, menor será la dependencia de Cundinamarca respecto a un grupo reducido de municipios." 
      
    } 
    #else {
    #  lugar_max <- res$lugar_max
    #  porcentaje_max <- res$porcentaje_max
    #  if (is.na(input$municipios) || is.null(input$municipios)){
    #    values$subtitulo <- "Por favor ingrese el numero de municipios que quiere graficar"
    #  } else {
    #    values$subtitulo <- paste0(lugar_max, " es el municipio con mayor importancia en el abastecimiento de Antioquia, aporta ", porcentaje_max, "%")
    #  }
    #}
    return(values$subtitulo)
  })
  
  
  
# Mensajes 
values <- reactiveValues(mensaje1 = NULL)
output$mensaje1 <- renderText({
  resultado_data <- resultado()
  if (nrow(resultado_data$datos) == 0) {
    validate("No hay información disponible")
  } else {
    values$mensaje1 <- paste("Se obtiene un coeficiente gini de",round(resultado_data$gini_,2),". Esta es una medida de dependencia en la que valores cercanos a 1 indican dependencia de unos pocos proveedores y valores cercanos a cero la dependencia es igual con todos los proveedores ")
   
  return(values$mensaje1)
  }
    })
  
  
# Boton de reset
  
 observeEvent(input$reset, {
      updateSelectInput(session, "anio", selected = "todo")
      updateSelectInput(session, "mes", selected = "todo")
      updateSelectInput(session, "producto", selected = NULL)
    })
 

# Descagamos el grafico
 
observeEvent(input$descargar, {
   screenshot("#grafico", scale = 5, file = "grafico_1.png")
 }) 

# Generamos el informe
  
output$report <- downloadHandler(
  filename = 'informe.pdf',
  
  content = function(file) {
    # Ruta al archivo RMarkdown
    rmd_file <- "informe.Rmd"

    # Renderizar el archivo RMarkdown a PDF
    rmarkdown::render(rmd_file, output_file = file, params = list(
      datos = resultado()$datos, 
      gini_ = resultado()$gini_, 
      plot = grafico_plano(),# Accede al gráfico 'grafico_plano'
      subtitulo = values$subtitulo,
      mensaje1 = values$mensaje1,
      anio = input$anio,
      mes = input$mes,
      alimento = input$producto
    ))
    
  
  },
  
  contentType = 'application/pdf'
)

}


