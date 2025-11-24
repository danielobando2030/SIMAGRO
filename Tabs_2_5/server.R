#Proyecto FAO
#INDICE Herfindahl–Hirschman - shiny 2 - De donde viene la comida (municipios)
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
# Funcion 8
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse); library(shiny); library(lubridate);library(shinythemes);library(shiny);library(shinyscreenshot);
options(scipen = 999)
################################################################################
rm(list = ls())

server <- function(input, output, session) {
  resultado <- reactive({
    tipo <- input$tipo
    anio_seleccionado <- input$anio
    productos_seleccionados <- input$producto
    
    if ((tipo == 2 || tipo == 4) && is.null(productos_seleccionados)) {
      validate(
        ("Debe seleccionar un producto.")
      )
    }
    
    if (is.null(productos_seleccionados)) {
      productos_seleccionados <- ""
    }
    
    if (tipo == 2) {
      grafica_indice(tipo, "", productos_seleccionados)
    } else if (tipo == 3) {
      if (is.null(anio_seleccionado) || anio_seleccionado == "todo"){
        grafica_indice(tipo)
      } else {
        grafica_indice(tipo, anio_seleccionado)
      }
    } else if (tipo == 4) {
      if (is.null(anio_seleccionado) || anio_seleccionado == "todo"){
        anio_seleccionado  <- ""
      }
      grafica_indice(tipo, anio_seleccionado, productos_seleccionados)
    } else {
      if (is.null(anio_seleccionado) || anio_seleccionado == "todo"){ 
        anio_seleccionado  <- ""
      }
      grafica_indice(tipo, anio_seleccionado, productos_seleccionados)
    }
  })
  
  
  
#  BOTON DE RESET  
  observeEvent(input$reset, {
    updateSelectInput(session, "tipo", selected = 1)
    updateSelectInput(session, "producto", selected = "")
    updateSelectInput(session, "anio", selected = "todo")
  })
  
  
# RENDER PLOTLY  
  output$grafico <- renderPlotly({
    plotly::ggplotly(resultado()$grafico)
  })
  
# RENDER PLOT
  grafico_plano <- reactive({
    res<-resultado()
    if(nrow(res$datos)==0){
      validate(
        ("No hay datos disponibles")
      )
    }else{
      res$grafico_plano
    }
  })   

# DESCARGAR PLOT 
output$descargar_ <- downloadHandler(
    filename = function() {
      paste("IND2_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- resultado()
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = res$grafico_plano, width = 13, height = 7, dpi = 200)
    }
  )
  
  
# HEAD DATOS  
  output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  
# DESCARGA DE DATOS
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  
  
# SUBTITULO  
values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL, mensaje2= NULL) 
  
output$subtitulo <- renderText({
  
  # Validación de producto cuando aplica
  if ((input$tipo == 2 || input$tipo == 4) && is.null(input$producto)) {
    return("Debe seleccionar un producto.")
  }
  
  # Manejo de año seleccionado
  anio <- ""
  if (!is.null(input$anio) && input$anio != "todo") {
    anio <- input$anio
  }
  
  # Resultado según tipo, año y producto
  resultado <- grafica_indice(input$tipo, anio, input$producto)
  
  tipo <- input$tipo
  max_vulnerabilidad <- resultado$max_vulnerabilidad
  fecha_max_vulnerabilidad <- as.character(resultado$fecha_max_vulnerabilidad)
  producto_max_vulnerabilidad <- resultado$producto_max_vulnerabilidad
  
  # Separar fecha
  componentes <- strsplit(fecha_max_vulnerabilidad, "-")[[1]]
  anio <- componentes[1]
  mes <- componentes[2]
  dia <- componentes[3]
  
  # Nombre del mes
  nombres_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                     "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  mes <- nombres_meses[as.integer(mes)]
  
  
  #### ---------------------------------------
  ####   GENERACIÓN DEL SUBTÍTULO MEJORADO
  #### ---------------------------------------
  
  if (tipo == 2) {
    # Anual por producto
    values$subtitulo <- paste0(
      "Para los filtros seleccionados, el nivel más bajo de diversidad de territorios conectados por el flujo de alimentos desde Cundinamarca se registró en ",
      anio,
      ", cuando el índice alcanzó un valor máximo de ",
      max_vulnerabilidad,
      " para el producto ",
      producto_max_vulnerabilidad,
      "."
    )
    
  } else if (tipo == 3) {
    # Mensual todos los productos
    values$subtitulo <- paste0(
      "Para los filtros seleccionados, el nivel más bajo de diversidad de territorios conectados por el flujo de alimentos desde Cundinamarca se observó en ",
      mes, " de ", anio,
      ", periodo en el que el índice alcanzó su valor máximo de ",
      max_vulnerabilidad,
      "."
    )
    
  } else if (tipo == 4) {
    # Mensual por producto
    values$subtitulo <- paste0(
      "Para los filtros seleccionados, el nivel más bajo de diversidad de territorios conectados por el flujo de alimentos desde Cundinamarca se presentó en ",
      mes, " de ", anio,
      ", alcanzando un índice máximo de ",
      max_vulnerabilidad,
      " para el producto ",
      producto_max_vulnerabilidad,
      "."
    )
    
  } else {
    # General
    values$subtitulo <- paste0(
      "Para los filtros seleccionados, el nivel más bajo de diversidad de territorios conectados por el flujo de alimentos desde Cundinamarca se presentó en ",
      anio,
      ", periodo en el que el índice alcanzó un valor máximo de ",
      max_vulnerabilidad,
      "."
    )
  }
  
  return(values$subtitulo)
})

# MENSAJES: MENSAJE 1   
  output$mensaje1 <- renderText({
    values$mensaje1 <-("Este índice permite medir el nivel de diversidad en los destinos económicos. Valores altos indican mayor concentración en pocos destinos (menor diversidad), mientras que valores bajos reflejan una distribución más equilibrada entre múltiples destinos (mayor diversidad).")
    values$mensaje1  
    })
#MENSAJE: MENSAJE 2  
  output$mensaje2 <- renderUI({
    values$mensaje2 <- ("El índice aumenta cuando pocos destinos concentran la mayor parte del volumen registrado, lo que refleja una menor diversidad. Por el contrario, disminuye cuando la distribución es más equilibrada entre varios destinos, indicando una mayor diversidad.")
    values$mensaje2
    })
  

  # GENERAMOS INFORME 
  output$report <- downloadHandler(
    filename = 'informe.pdf',
    
    content = function(file) {
      # Ruta al archivo RMarkdown
      rmd_file <- "informe.Rmd"
      
      # Renderizar el archivo RMarkdown a PDF
      rmarkdown::render(rmd_file, output_file = file, params = list(
        tipo = input$tipo,
        producto= input$producto,
        anio = input$anio,
        plot = grafico_plano(),
        subtitulo = values$subtitulo,
        mensaje1 = values$mensaje1,
        mensaje2= values$mensaje2
        
      ))  
    },
    contentType = 'application/pdf'
  )    
  
  
  
  }



