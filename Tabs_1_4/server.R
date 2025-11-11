#Proyecto FAO
#Procesamiento datos SIPSA
# Server
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 02/04/2024
#Fecha de ultima modificacion: 02/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
options(scipen = 999)
################################################################################-

# Definir la función de servidor
server <- function(input, output, session) {
  
  resultado<-reactive({
    # Comprobar si solo se ha seleccionado un producto
    if (input$producto != "todo" && input$anio == "todo" && input$mes == "todo") {
      importancia(Producto = input$producto,municipios = input$municipios)
    } else if (input$mes != "todo" && input$anio == "todo") {
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    } else if(input$anio == "todo" && input$producto == "todo" && input$mes == "todo"){
      importancia(municipios = input$municipios)
    } else if(input$producto == "todo" && input$mes == "todo" ){
      importancia(Año = input$anio, municipios = input$municipios)
    } else if(input$producto == "todo"){
      importancia(Año = input$anio, Mes = input$mes ,municipios = input$municipios)
    } else if(input$mes == "todo" ){
      importancia(Año = input$anio, municipios = input$municipios, Producto = input$producto)
    } else if(input$anio == "todo" && input$mes == "todo"){
      importancia(Producto = input$producto,municipios = input$municipios)
    } else{
      importancia(Año = input$anio, Mes = input$mes ,municipios = input$municipios, Producto = input$producto)
    }
  })
  
  # Render Grafico
  output$grafico <- plotly::renderPlotly({
    res <- resultado()
    if (nrow(res$datos) == 0) {
      validate("No hay datos disponibles"
      )
    } else {
      res$grafico
    }
  })
  
  # Render Grafico plano
  grafico_plano <- reactive({
    res <- resultado()
    if (is.character(res) || length(res) == 0 || is.null(input$municipios) || input$municipios < 1) {
      return(NULL)  # No hay gráfico para guardar
    } else {
      res$grafico_plano  # Guarda solo el gráfico 'grafico_plano'
    }
  }) 
  
  # Head tabla 
  output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  

  # Descarga de imagen 
  
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
  
  
  
  # Descarga de datos
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  
  
  # Boton de reset 
  observeEvent(input$reset, {
    updateSelectInput(session, "municipios", selected = 10)
    updateSelectInput(session, "anio", selected = "todo")
    updateSelectInput(session, "mes", selected = "todo")
    updateSelectInput(session, "producto", selected = "todo")
  })
  
  
  # Mensajes
  values <- reactiveValues(mensaje1 = NULL)
  output$mensaje1 <- renderText({
    resultado_data <- resultado()
    if (nrow(resultado_data$datos) == 0) {
      validate("No hay información disponible")
    } else {
      values$mensaje1<- (paste0("El ", round(resultado_data$porcentaje_max, digits = 1), "% de los productos procedentes de municipios de Cundinamarca tienen como destino los principales centros de abasto de ", resultado_data$lugar_max, "."))
    }
    return(values$mensaje1)
  })

  
  # Generamos el informe
  
  output$report <- downloadHandler(
    filename = 'informe.pdf',
    
    content = function(file) {
      # Ruta al archivo RMarkdown
      rmd_file <- "informe.Rmd"
      
      # Renderizar el archivo RMarkdown a PDF
      rmarkdown::render(rmd_file, output_file = file, params = list(
      anio = input$anio,
      mes = input$mes,
      alimento = input$producto,
      municipios = input$municipios,
      plot = resultado()$grafico_plano,
      mensaje1 = values$mensaje1
      ))
      
      
    },
    
    contentType = 'application/pdf'
  )
  
  
}

