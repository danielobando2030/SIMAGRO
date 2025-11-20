#Proyecto FAO
#INDICE Herfindahl–Hirschman - Abastecimiento shiny abasteciemiento
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 14/03/2024
################################################################################
# Paquetes 
################################################################################
library(shiny); library(lubridate);library(shinythemes);library(plotly);
library(shinydashboard)
options(scipen = 999)
################################################################################

server <- function(input, output, session) {
  resultado <- reactive({
    # Si el input del año está vacío o es "todo", usa "todo", de lo contrario, usa el año seleccionado
    anio <- if (is.null(input$anio) || input$anio == "todo") NULL else input$anio
    tipo <- input$tipo
    
    # Llama a la función plot_data con los parámetros seleccionados
    plot_data(tipo, anio)
  })

  
  # Rendre plotly
  output$grafico1 <- plotly::renderPlotly({
    resultado()$plot
  })
  
  # Render grafico plano
  
  grafico_plano <- reactive({
    res<-resultado()
    if(nrow(res$data)==0){
      validate(
        ("No hay datos disponibles")
      )
    }else{
      res$grafico_plano
    }
  })  
  
  # Descargamos la grafica
  
  # Descargar el grafico 
  output$descargar_ <- downloadHandler(
    filename = function() {
      paste("Ind1", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- resultado()
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = res$grafico_plano, width = 13, height = 7, dpi = 200)
    })  
  
  
  # head de los datos    
  output$vistaTabla <- renderTable({
    if (!is.null(resultado()$data)) {
      head(resultado()$data, 5)
    }
  })
  
  # Descargar datos   
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$data, file)
    }
  )
  
  
  values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL, mensaje2=NULL)
  
  # Generar subtitulo dinamico
  output$subtitulo <- renderText({
    tipo <- input$tipo
    anio <- ifelse(is.null(input$anio) || input$anio == "todo", NA, input$anio)
    data_resultado <- resultado()
    
    max_IHH <- round(data_resultado$max_IHH,digits = 1)
    mes_max_IHH <- data_resultado$mes_max_IHH
    anio_max_IHH <- data_resultado$anio_max_IHH
    
    # Crear un vector con los nombres de los meses en español
    meses_es <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    # Crear un vector con las abreviaturas de los meses en español
    abrev_meses_es <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
    
    # Convertir la abreviatura del mes a un nombre completo de mes
    mes_max_IHH <- meses_es[match(tolower(mes_max_IHH), abrev_meses_es)]
    
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
    
    if (tipo == 1) {
      values$subtitulo <- paste(
        "El año con la menor diversidad de alimentos fue",
        anio_max_IHH,
        "al registrar un índice de",
        max_IHH, ". Sin embargo, esto corresponde a", categoria_hhi, "."
      )
    } else if (tipo == 0) {
      values$subtitulo <- paste(
        "El mes con la menor diversidad de alimentos fue",
        mes_max_IHH, "del año", anio_max_IHH,
        "al registrar un índice de",
        max_IHH, ", correspondiente a", categoria_hhi, "."
      )
    }
    return(values$subtitulo)
  })
  
  
  # Borrar filtros
  observeEvent(input$reset, {
    updateSelectInput(session, "tipo", selected = 1)
  })
  
  # Mensajes: Mensaje 1  
  output$mensaje1 <- renderText({
    values$mensaje1 <- ("Este índice muestra el nivel de concentración de los alimentos que ingresan a Cundinamarca.
Valores altos indican menor diversidad; valores bajos reflejan una mayor variedad")
    values$mensaje1
  })
  
  # Mensajes: Mensaje 2
  output$mensaje2 <- renderUI({
    values$mensaje2 <-("El índice aumenta cuando el volumen ingresado se concentra en unos pocos alimentos, reflejando una menor diversidad.")
    values$mensaje2
  })
  
  # Generamos el Informe
  output$report <- downloadHandler(
    filename = 'informe.pdf',
    
    content = function(file) {
      # Ruta al archivo RMarkdown
      rmd_file <- "informe.Rmd"
      
      # Renderizar el archivo RMarkdown a PDF
      rmarkdown::render(rmd_file, output_file = file, params = list(
        tipo = input$tipo,
        anio = input$anio,
        subtitulo = values$subtitulo,
        plot = grafico_plano(),
        mensaje1 = values$mensaje1,
        mensaje2 = values$mensaje2
      ))  
    },
    contentType = 'application/pdf'
  )   
  
  
}