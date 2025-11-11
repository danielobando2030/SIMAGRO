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
    
    if (tipo == 1) {
      values$subtitulo <- (paste("La menor variedad de alimentos registrada fue en el año", anio_max_IHH, "donde se registró un índice máximo de", max_IHH))
    } else if (tipo == 0) {
      values$subtitulo <- (paste("La menor variedad de alimentos registrada fue en el mes", mes_max_IHH,"del año", anio_max_IHH, "donde se registró un índice máximo de", max_IHH))
    }
    return(values$subtitulo)
  })
  
  
  # Borrar filtros
  observeEvent(input$reset, {
    updateSelectInput(session, "tipo", selected = 1)
  })
  
  # Mensajes: Mensaje 1  
  output$mensaje1 <- renderText({
    values$mensaje1 <- ("El índice de Herfindahl-Hirschman permite conocer el nivel de concentración de los alimentos en Cundinamarca, un mayor índice refleja menos variedad de alimentos")
    values$mensaje1
  })
  
  # Mensajes: Mensaje 2
  output$mensaje2 <- renderUI({
    values$mensaje2 <-("Este índice puede aumentar si un producto incrementa su participación en el volumen total o si disminuye el número de productos que ingresan
")
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