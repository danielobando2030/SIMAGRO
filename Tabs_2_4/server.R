#Proyecto FAO
#INDICE Herfindahl–Hirschman - shiny 2 - De donde viene la comida (municipios)
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
# Tablero 7
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse); library(shiny); library(lubridate);library(shinythemes);library(plotly);library(shinyscreenshot);
options(scipen = 999)
################################################################################
server <- function(input, output, session) {
  resultado <- reactive({
    tipo <- input$tipo
    anio_seleccionado <- input$anio
    productos_seleccionados <- input$producto
    
    if ((tipo == 2 || tipo == 4) && (is.null(productos_seleccionados) || length(productos_seleccionados) == 0)) {
      validate(
        need(FALSE, "Debe seleccionar un producto.")
      )
    }
    
    if (is.null(productos_seleccionados))  {
      productos_seleccionados <- ""
    }
    if (tipo == 2) {
      grafica_indice_mun(tipo, "", productos_seleccionados)
    } else if (tipo == 3) {
      if (is.null(anio_seleccionado) || anio_seleccionado =="todo"){
        grafica_indice_mun(tipo)
      } else {
        grafica_indice_mun(tipo, anio_seleccionado)
      }
    } else if (tipo == 4) {
      if (is.null(anio_seleccionado) || anio_seleccionado == "todo"){
        anio_seleccionado  <- ""
      }
      grafica_indice_mun(tipo, anio_seleccionado, productos_seleccionados)
    } else {
      if (is.null(anio_seleccionado) || anio_seleccionado == "todo"){
        anio_seleccionado  <- ""
      }
      grafica_indice_mun(tipo, anio_seleccionado, productos_seleccionados)
    }
  })
  
  
# Render plotly
  output$grafico <- plotly::renderPlotly({
    resultado()$grafico
  })
  
# Render graf plano
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
  
# Descargar grafica

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

# HEAD DE LOS DATOS

output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  
 
# Descargar base de datos
output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos_importancia_mpios", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
)
  
  
# SUBTITULO
values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL, mensaje2= NULL)  
# En el servidor
output$subtitulo <- renderText({
  
  # Validación de producto para tipo 2 y 4
  if ((input$tipo == 2 || input$tipo == 4) && is.null(input$producto)) {
    return("Debe seleccionar un producto.")
  }
  
  tipo <- input$tipo
  anio <- ifelse(is.null(input$anio) || input$anio == "todo", NA, input$anio)
  
  # Ejecutar función que trae resultados
  resultado <- grafica_indice_mun(tipo, anio, input$producto)
  
  max_IHH <- round(resultado$max_vulnerabilidad, 1)
  fecha <- as.character(resultado$fecha_max_vulnerabilidad)
  producto_max <- resultado$producto_max_vulnerabilidad
  
  # Separar en año-mes-día
  partes <- strsplit(fecha, "-")[[1]]
  anio_max_IHH <- partes[1]
  mes_num <- partes[2]
  
  # Meses completos
  meses_es <- c("enero","febrero","marzo","abril","mayo","junio",
                "julio","agosto","septiembre","octubre","noviembre","diciembre")
  mes_max_IHH <- meses_es[as.integer(mes_num)]
  
  # Clasificación del HHI
  clasificar_hhi <- function(hhi) {
    if (hhi <= 1500) {
      return("baja concentración del volumen de los alimentos (alta diversidad)")
    } else if (hhi <= 2500) {
      return("concentración moderada del volumen de los alimentos (diversidad media)")
    } else {
      return("alta concentración del volumen de los alimentos (baja diversidad)")
    }
  }
  
  categoria_hhi <- clasificar_hhi(max_IHH)
  
  # -------------------------------
  # Textos por tipo
  # -------------------------------
  
  if (tipo == 2) {
    values$subtitulo <- paste(
      "El año con la menor diversidad de territorios conectados hacia Cundinamarca fue",
      anio_max_IHH,
      "al registrar un índice de",
      max_IHH, "para el producto", producto_max, 
      ". Esto corresponde a", categoria_hhi, "."
    )
    
  } else if (tipo == 3) {
    values$subtitulo <- paste(
      "El mes con la menor diversidad de territorios conectados hacia Cundinamarca fue",
      mes_max_IHH, "del año", anio_max_IHH,
      "al registrar un índice de",
      max_IHH, ", correspondiente a", categoria_hhi, "."
    )
    
  } else if (tipo == 4) {
    values$subtitulo <- paste(
      "El mes con la menor diversidad de territorios conectados hacia Cundinamarca fue",
      mes_max_IHH, "del año", anio_max_IHH,
      "al registrar un índice de",
      max_IHH, "para el producto", producto_max,
      ". Esto corresponde a", categoria_hhi, "."
    )
    
  } else {
    # tipo 1 o cualquier otro
    values$subtitulo <- paste(
      "El año con la menor diversidad de territorios conectados hacia Cundinamarca fue",
      anio_max_IHH,
      "al registrar un índice de",
      max_IHH, ". Esto corresponde a", categoria_hhi, "."
    )
  }
  
  return(values$subtitulo)
})

  
  # Borrar filtros
  observeEvent(input$reset, {
    updateSelectInput(session, "tipo", selected = 1)
  })
  
# Mensaje: MENSAJE 1  
  output$mensaje1 <- renderText({
    values$mensaje1 <- ("Este índice muestra el nivel de concentración de los origenes de los alimentos que ingresan a Cundinamarca.
Valores altos indican menor diversidad de origenes; valores bajos indican mayor diversidad de origenes")
    values$mensaje1
    })
  
# Mensaje: MENSAJE 2
  output$mensaje2 <- renderUI({
    values$mensaje2 <-("El índice aumenta cuando pocos municipios de origen concentran la mayor parte del volumen de alimentos que ingresa a Cundinamarca, disminuye cuando el abastecimiento depende de muchos origenes.")
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
        mensaje2 = values$mensaje2

      ))  
    },
    contentType = 'application/pdf'
  )    
  
  
}