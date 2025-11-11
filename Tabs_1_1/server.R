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
    if (is.null(input$municipios) || is.na(input$municipios) || input$municipios == 0) {
      return("No hay información disponible")
    }
    # Comprobar si solo se ha seleccionado un producto
    if (input$producto != "todo" && input$anio == "todo" && input$mes == "todo") {
      importancia(tipo = input$variable, Producto = input$producto,municipios = input$municipios)
    } else if (input$mes != "todo" && input$anio == "todo") {
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    } else if(input$anio == "todo" && input$producto == "todo" && input$mes == "todo"){
      importancia(tipo = input$variable, municipios = input$municipios)
    } else if(input$producto == "todo" && input$mes == "todo" ){
      importancia(tipo = input$variable, Año = input$anio, municipios = input$municipios)
    } else if(input$producto == "todo"){
      importancia(tipo = input$variable, Año = input$anio, Mes = input$mes ,municipios = input$municipios)
    } else if(input$mes == "todo" ){
      importancia(tipo = input$variable, Año = input$anio, municipios = input$municipios, Producto = input$producto)
    } else if(input$anio == "todo" && input$mes == "todo"){
      importancia(tipo = input$variable, Producto = input$producto)
    } else{
      importancia(tipo = input$variable, Año = input$anio, Mes = input$mes ,municipios = input$municipios, Producto = input$producto)
    }
  })
  

# Se genera el grafico plotly 
  
  output$grafico <- plotly::renderPlotly({
    res <- resultado()
    if (is.character(res) || length(res) == 0 || is.null(input$municipios) || input$municipios < 1) {
      return(NULL)  #  No hay gráfico para mostrar
    } else {
      res$grafico_plotly  # Devuelve el gráfico Plotly
    }
  })
  
# Se genera el grafico plano
  
  grafico_plano <- reactive({
    res <- resultado()
    if (is.character(res) || length(res) == 0 || is.null(input$municipios) || input$municipios < 1) {
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
    #return(values$subtitulo)
  })
  
  
  
# Mensajes 
values <- reactiveValues(mensaje1 = NULL)
output$mensaje1 <- renderText({
  resultado_data <- resultado()
  if (nrow(resultado_data$datos) == 0) {
    validate("No hay información disponible")
  } else {
  if ( is.null(input$municipios) || input$municipios < 1) {
    values$mensaje1 <- "No hay información disponible"
  } else if (input$variable == 1) {
    values$mensaje1 <- paste0(resultado()$lugar_max, " se destaca como el principal proveedor de alimentos en las centrales de abasto de Bogotá, representando el ", round(resultado()$porcentaje_max,digits = 1), " % del volumen total.")
  } else if (input$variable == 2){
    values$mensaje1 <- paste0(resultado()$lugar_max, " se destaca como el principal proveedor de alimentos en las centrales de abasto de Bogotá, representando el ", round(resultado()$porcentaje_max, digits = 1), " % del volumen local.")
  } else{
    values$mensaje1 <- paste0(resultado()$lugar_max, " se destaca como el principal proveedor de alimentos en las centrales de abasto de Bogotá, representando el ", round(resultado()$porcentaje_max, digits = 1), " % del volumen externo.")
  }
  return(values$mensaje1)
}})
  
  
# Boton de reset
  
 observeEvent(input$reset, {
      updateSelectInput(session, "municipios", selected = 10)
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
      lugar_max = resultado()$lugar_max, 
      porcentaje_max = resultado()$porcentaje_max, 
      plot = grafico_plano(),# Accede al gráfico 'grafico_plano'
      subtitulo = values$subtitulo,
      mensaje1 = values$mensaje1,
      tipo = input$variable,
      anio = input$anio,
      mes = input$mes,
      alimento = input$producto
    ))
    
  
  },
  contentType = 'application/pdf'
)

}


