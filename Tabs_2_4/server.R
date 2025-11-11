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
  if ((input$tipo == 2 || input$tipo == 4) && is.null(input$producto)) {
    return("Debe seleccionar un producto.")
  }
  # Lógica para manejar años no seleccionados
  if (is.null(input$anio) || input$anio == "todo") {
    anio <- ""
  } else {
    anio <- input$anio
  }
  
  resultado <- grafica_indice_mun(input$tipo, anio, input$producto)
  tipo <- input$tipo
  max_IHH <- resultado$max_vulnerabilidad
  fecha_max_vulnerabilidad <- resultado$fecha_max_vulnerabilidad
  producto_max_vulnerabilidad <- resultado$producto_max_vulnerabilidad
  fecha_max_vulnerabilidad <- as.character(fecha_max_vulnerabilidad)
  componentes <- strsplit(fecha_max_vulnerabilidad, "-")[[1]]
  anio <- componentes[1]
  mes <- componentes[2]
  dia <- componentes[3]
  nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                     "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  mes <- nombres_meses[as.integer(mes)]
  producto_max_vulnerabilidad <- resultado$producto_max_vulnerabilidad
  
  if (tipo == 2) {
    values$subtitulo <- (paste("La menor variedad de territorios conectados por el flujo de alimentos desde otros territorios hacia Cundinamarca se registró en el" ,anio," con un índice máximo de" , max_IHH, " para el producto ",producto_max_vulnerabilidad ))
  } else if (tipo == 3) {
    values$subtitulo <- (paste("La menor variedad de territorios conectados por el flujo de alimentos desde otros territorios hacia Cundinamarca se registró en ",mes, " del año ",anio, "con un índice máximo de", max_IHH ))
  } else if (tipo == 4) {
    values$subtitulo <- (paste("La menor variedad de territorios conectados por el flujo de alimentos desde otros territorios hacia Cundinamarca se registró en ",mes, " del año ",anio, " con un índice máximo ", max_IHH, "para el producto" ,producto_max_vulnerabilidad))
  } else {
    values$subtitulo <-(paste("La menor variedad de territorios conectados por el flujo de alimentos desde otros territorios hacia Cundinamarca se registró en el ",anio," con un índice máximo de", max_IHH ))
  }
  return(values$subtitulo)
})

  
  # Borrar filtros
  observeEvent(input$reset, {
    updateSelectInput(session, "tipo", selected = 1)
  })
  
# Mensaje: MENSAJE 1  
  output$mensaje1 <- renderText({
    values$mensaje1 <- ("El índice de Herfindahl-Hirschman permite midir la concentración de los orígenes de alimentos. Un índice más alto indica que hay menos municipios de origen para los alimentos que llegan a las principales plazas de abasto de Cundinamarca")
    values$mensaje1
    })
  
# Mensaje: MENSAJE 2
  output$mensaje2 <- renderUI({
    values$mensaje2 <-("Este índice puede aumentar si incrementa la participación de un municipio sobre el volumen total o disminuye el número de municipios de origen")
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