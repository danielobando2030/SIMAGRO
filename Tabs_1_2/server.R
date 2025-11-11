#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 03/04/2024
#Fecha de ultima modificacion: 03/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
options(scipen = 999)
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
library(sf) 
################################################################################-
# Definir la función de servidor
server <- function(input, output, session) {
  
  resultado<-reactive({
    if (input$producto != "todo" && input$anio == "todo" && input$mes == "todo") {
      cun_en_col(Producto = input$producto)
    } else if (input$mes != "todo" && input$anio == "todo") {
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    } else if(input$anio == "todo" && input$producto == "todo" && input$mes == "todo"){
      cun_en_col()
    } else if(input$producto == "todo" && input$mes == "todo" ){
      cun_en_col(Año = input$anio)
    } else if(input$producto == "todo"){
      cun_en_col(Año = input$anio, Mes = input$mes)
    } else if(input$mes == "todo" ){
      cun_en_col(Año = input$anio, Producto = input$producto)
    } else if(input$anio == "todo" && input$mes == "todo"){
      cun_en_col(Producto = input$producto)
    } else{
      cun_en_col(Año = input$anio, Mes = input$mes ,Producto = input$producto)
    }
  })
  
  # Boton de reset
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "todo")
    updateSelectInput(session, "mes", selected = "todo")
    updateSelectInput(session, "anio", selected = "todo")
  })
  
#Generamos graf plotly  
  
  output$grafico <- renderLeaflet({
    res <- resultado()
    
    if (is.data.frame(res) || is.list(res)) {
      if (nrow(res$datos) == 0) {
        print("No hay datos disponibles")
        leaflet()  # Devuelve un mapa de Leaflet vacío
      } else {
        res$grafico %>%
          setView(lng = -75.5, lat = 3.9, zoom = 5) 
      }
    } else {
      print("No hay datos disponibles")
      leaflet()  # Devuelve un mapa de Leaflet vacío
    }
  })
  
  
  # Se genera el grafico plano
  grafico_plano <- reactive({
    res <- resultado()
    if (nrow(res$datos) == 0) {
      return(NULL)  # No hay gráfico para guardar
    } else {
      res$grafico_plano  # Guarda solo el gráfico 'grafico_plano'
    }
  }) 
  
  # Descargar el grafico 
  output$descargar_ <- downloadHandler(
    filename = function() {
      paste("grafica_productos_ingresan_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- resultado()
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = res$grafico_plano, width = 13, height = 7, dpi = 200)
    }
  ) 
  
  
  # Ver Tabla
  output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  
  
# Desacargar Datos
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  
# Subtitulo 
  
values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL)   

output$subtitulo <- renderText({
  res <- resultado()
  if (is.data.frame(res) || is.list(res)) {
    if(nrow(res$datos) == 0) {
      values$subtitulo <- ("No hay datos disponibles")
    } else {
      porcentaje_max <- res$porcentaje_max
      dpto_max <- res$dpto_max
      if (input$producto == "todo") {
        values$subtitulo <- (paste0("Cundinamarca es uno de los principales receptores de ", dpto_max," con un procentaje de ", porcentaje_max ,"%."))
      } else if (input$producto != "todo") {
        values$subtitulo <- (paste0("Cundinamarca es uno de los principales receptores de ", dpto_max," con un procentaje de ", porcentaje_max ,"%."))
      }
    }
  }
  return(values$subtitulo) 
})
  
 
# Mensajes
output$mensaje1 <- renderText({
  res <- resultado()
  if (is.data.frame(res) || is.list(res)) {
  if(nrow(res$datos) == 0) {
  values$mensaje1 <- ("No hay datos disponibles")
  }else {
  porcentaje_max <- res$porcentaje_max_1
  dpto_max <- res$dpto_max
  values$mensaje1 <- (paste0("El ", res$porcentaje_max_1, "% del volumen total",ifelse(input$producto == "todo"," de alimentos que reportan como origen los territorios de Cundinamarca",paste0(" de ",input$producto," que reportan como origen los territorios de Cundinamarca"))," llega a las principales centrales de abasto de Bogotá."))}}
  return(values$mensaje1)
  })
  

# Generamos el Informe
output$report <- downloadHandler(
  filename = 'informe.pdf',
  
  content = function(file) {
    # Ruta al archivo RMarkdown
    rmd_file <- "informe.Rmd"
    
    # Renderizar el archivo RMarkdown a PDF
    rmarkdown::render(rmd_file, output_file = file, params = list(
      anio = input$anio,
      mes = input$mes,
      producto = input$producto,
      subtitulo = values$subtitulo,
      maximo = resultado()$porcentaje_max_1,
      plot = grafico_plano(),
      mensaje1 = values$mensaje1
    ))  
  },
  contentType = 'application/pdf'
)  

  
  
  
}